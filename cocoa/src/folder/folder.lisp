(defpackage :cocoa.folder
  (:use :cl)
  (:import-from :cl-arrows :-> :->> :-<>))
(in-package :cocoa.folder)

(defun thumbnail->resp (thumbnail)
  (list :id (cocoa.entity.folder:thumbnail->image-id thumbnail)))

(defun folder->resp (folder)
  (list :id (cocoa.entity.folder:folder-id folder)
        :name (cocoa.entity.folder:folder-name folder)
        :thumbnail (thumbnail->resp (cocoa.entity.folder:folder-thumbnail
                                     folder))))
;;;; Get
(defun get-folder (db id)
  (cocoa.folder.util:accept-folder-id id)
  (->> (car (cocoa.entity.folder.repository:load-by-ids db (list id)))
       folder->resp))
(export 'get-folder)


;;;; Update
#+nil
(defun update-thumbnail (db folder-id &key image-id)
  (cocoa.folder.util:accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder.repository:load-by-ids
                      db
                      (list folder-id)))))
    (setf (cocoa.entity.folder:folder-thumbnail folder)
          (cocoa.entity.folder:make-thumbnail/image image-id))
    (cocoa.entity.folder.repository:update db folder)))

;;;; Delete
(defun delete-bulk (db folder-ids)
  (mapc #'cocoa.folder.util:accept-folder-id folder-ids)
  (let ((folders (cocoa.entity.folder.repository:load-by-ids db
                  folder-ids)))
    ;; Delete thumbnails
    (cocoa.entity.fs.image.repository:delete-bulk db
     (mapcar (alexandria:compose
              #'cocoa.entity.folder:thumbnail->image-id
              #'cocoa.entity.folder:folder-thumbnail)
             folders))
    ;; Delete contents
    (dolist (folder folders)
      (let ((contents
             (cocoa.entity.folder.content.repository:folder-contents db
              folder)))
        (let ((image-ids
               (mapcar #'cocoa.entity.folder.content:content->image-id
                       contents)))
          (cocoa.entity.fs.image.repository:delete-bulk db image-ids))))
    ;; Delete content relations
    (let ((deleting-bulk
           (cocoa.entity.folder.content.op:make-deleting-bulk
            :folders folders)))
      (cocoa.entity.folder.content.repository:update db deleting-bulk)))
  ;; Delete folders
  (cocoa.entity.folder.repository:delete-by-ids db folder-ids))
(export 'delete-bulk)

;;;; Add
;;; The representation of a directory in the local file system
(defstruct dir path image-paths modified-at)
(export 'make-dir)



(defun create-image (id-generator path)
  (let ((image-id (cocoa.entity.id:gen id-generator path)))
    (cocoa.entity.fs.image:make-image image-id path)))

(defun create-images-bulk (id-generator paths)
  (mapcar (alexandria:curry #'create-image id-generator) paths))


(defun add-dir-thumbnail (db dir &key make-thumbnail-file-fn id-generator)
  (let ((thumbnail-source-path (car (dir-image-paths dir))))
    (let ((thumbnail-path (funcall make-thumbnail-file-fn
                                   thumbnail-source-path)))
      (let ((image (create-image id-generator thumbnail-path)))
        (let ((image-id (cocoa.entity.fs.image:image-id image)))
          (cocoa.entity.fs.image.repository:delete-bulk db (list image-id))
          (cocoa.entity.fs.image.repository:add-bulk db (list image))
          (cocoa.entity.folder:make-thumbnail/image image-id))))))

(defun add-dir-contents (db dir &key id-generator)
  (let ((images (create-images-bulk id-generator (dir-image-paths dir))))
    (let ((image-ids (mapcar #'cocoa.entity.fs.image:image-id images)))
      (cocoa.entity.fs.image.repository:delete-bulk db image-ids)
      (cocoa.entity.fs.image.repository:add-bulk db images)
      (mapcar #'cocoa.entity.folder.content:make-content/image image-ids))))

(defun add-bulk (db dirs &key id-generator make-thumbnail-file-fn)
  (labels ((dir-folder (dir)
             (cocoa.entity.folder:make-folder
              :id (cocoa.entity.id:gen id-generator (dir-path dir))
              :name (dir-path dir)
              :thumbnail (add-dir-thumbnail db dir
                          :id-generator id-generator
                          :make-thumbnail-file-fn make-thumbnail-file-fn)
              :modified-at (dir-modified-at dir))))
    ;; Create folders
    (let ((folders (mapcar #'dir-folder dirs)))
      ;; Delete existing folders if any
      (delete-bulk db (mapcar #'cocoa.entity.folder:folder-id folders))
      ;; Save folders
      (cocoa.entity.folder.repository:save-bulk db folders)
      ;; Append new contents
      (let ((appendings
             (loop for dir in dirs
                   for folder in folders
                   for contents = (add-dir-contents db dir
                                   :id-generator id-generator)
                   collect (cocoa.entity.folder.content.op:make-appending
                            :folder folder
                            :contents contents))))
        (let ((appending-bulk
               (cocoa.entity.folder.content.op:make-appending-bulk
                :appendings appendings)))
          (cocoa.entity.folder.content.repository:update
           db appending-bulk)))
      (mapcar #'folder->resp folders))))
(export 'add-bulk)

#+nil
(defun add (db name &key thumbnail id-generator)
  (let ((folder (cocoa.entity.folder:make-folder
                 :id (cocoa.entity.id:gen id-generator name)
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
    (cocoa.entity.folder.repository:save-bulk db (list folder))
    (folder->resp folder)))
