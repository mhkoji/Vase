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
#+nil
(defun delete-folder (db folder-id)
  (cocoa.folder.util:accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder.repository:load-by-ids
                      db
                      (list folder-id)))))
    (let ((deleting-bulk
           (cocoa.entity.folder.content.op:make-deleting-bulk
            :folders (list folder))))
      (cocoa.entity.folder.content.repository:update db deleting-bulk)))
  (cocoa.entity.folder.repository:delete-by-ids db (list folder-id)))

;;;; Add
;;; The representation of a directory in the local file system
(defstruct dir path image-paths modified-at)
(export 'make-dir)

(defun dir-thumbnail (db dir &key make-thumbnail-file id-generator)
  (let ((thumbnail-path (funcall make-thumbnail-file
                                 (car (dir-image-paths dir)))))
    (let ((image-id (cocoa.entity.id:gen id-generator thumbnail-path)))
      (let ((image (cocoa.entity.fs.image:make-image image-id
                                                     thumbnail-path)))
        (cocoa.entity.fs.image.repository:save-bulk db (list image)))
      (cocoa.entity.folder:make-thumbnail/image image-id))))

(defun dir-contents (db dir &key id-generator)
  (let ((image-ids (mapcar (alexandria:curry #'cocoa.entity.id:gen
                                             id-generator)
                           (dir-image-paths dir))))
    (let ((images (mapcar #'cocoa.entity.fs.image:make-image
                          image-ids
                          (dir-image-paths dir))))
      (cocoa.entity.fs.image.repository:save-bulk db images))
    (mapcar #'cocoa.entity.folder.content:make-content/image image-ids)))

(defun add-bulk (db dirs &key id-generator make-thumbnail-file)
  (labels ((dir-folder (dir)
             (cocoa.entity.folder:make-folder
              :id (cocoa.entity.id:gen id-generator (dir-path dir))
              :name (dir-path dir)
              :thumbnail (dir-thumbnail db dir
                          :make-thumbnail-file make-thumbnail-file
                          :id-generator id-generator)
              :modified-at (dir-modified-at dir))))
    ;; Create folders
    (let ((folders (mapcar #'dir-folder dirs)))
      ;; Save folders
      (cocoa.entity.folder.repository:save-bulk db folders)
      ;; Delete existing contents if any
      (let ((deleting-bulk
             (cocoa.entity.folder.content.op:make-deleting-bulk
              :folders folders)))
        (cocoa.entity.folder.content.repository:update db deleting-bulk))
      ;; Append new contents
      (let ((appendings
             (loop for dir in dirs
                   for folder in folders
                   for contents = (dir-contents db dir
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
