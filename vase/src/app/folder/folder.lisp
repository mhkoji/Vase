(defpackage :vase.app.folder
  (:use :cl)
  (:import-from :cl-arrows :-> :->> :-<>))
(in-package :vase.app.folder)

(defun thumbnail->resp (thumbnail)
  (list :id (vase.entities.folder:thumbnail->image-id thumbnail)))

(defun folder->resp (folder)
  (list :id (vase.entities.folder:folder-id folder)
        :name (vase.entities.folder:folder-name folder)
        :thumbnail (thumbnail->resp (vase.entities.folder:folder-thumbnail
                                     folder))))
;;;; Get
(defun get-folder (db id)
  (vase.app.folder.util:accept-folder-id id)
  (->> (car (vase.entities.folder.repository:load-by-ids db (list id)))
       folder->resp))
(export 'get-folder)


;;;; Update
#+nil
(defun update-thumbnail (db folder-id &key image-id)
  (vase.app.folder.util:accept-folder-id folder-id)
  (let ((folder (car (vase.entities.folder.repository:load-by-ids
                      db
                      (list folder-id)))))
    (setf (vase.entities.folder:folder-thumbnail folder)
          (vase.entities.folder:make-thumbnail/image image-id))
    (vase.entities.folder.repository:update db folder)))

;;;; Delete
(defun delete-bulk (db folder-ids)
  (mapc #'vase.app.folder.util:accept-folder-id folder-ids)
  (let ((folders (vase.entities.folder.repository:load-by-ids db
                  folder-ids)))
    ;; Delete thumbnails
    (vase.entities.fs.image.repository:delete-bulk db
     (mapcar (alexandria:compose
              #'vase.entities.folder:thumbnail->image-id
              #'vase.entities.folder:folder-thumbnail)
             folders))
    ;; Delete contents
    (dolist (folder folders)
      (let ((contents
             (vase.entities.folder.content.repository:folder-contents db
              folder)))
        (let ((image-ids
               (mapcar #'vase.entities.folder.content:content->image-id
                       contents)))
          (vase.entities.fs.image.repository:delete-bulk db image-ids))))
    ;; Delete content relations
    (let ((deleting-bulk
           (vase.entities.folder.content.op:make-deleting-bulk
            :folders folders)))
      (vase.entities.folder.content.repository:update db deleting-bulk)))
  ;; Delete folders
  (vase.entities.folder.repository:delete-by-ids db folder-ids))
(export 'delete-bulk)

;;;; Add
;;; The representation of a directory in the local file system
(defstruct dir path image-paths modified-at)
(export 'make-dir)


(defun create-image (id-generator path)
  (let ((image-id (vase.entities.id:gen id-generator path)))
    (vase.entities.fs.image:make-image image-id path)))

(defun create-images-bulk (id-generator paths)
  (mapcar (alexandria:curry #'create-image id-generator) paths))


(defun add-dir-thumbnail (db dir &key make-thumbnail-file-fn id-generator)
  (let ((thumbnail-source-path (car (dir-image-paths dir))))
    (let ((thumbnail-path (funcall make-thumbnail-file-fn
                                   thumbnail-source-path)))
      (let ((image (create-image id-generator thumbnail-path)))
        (let ((image-id (vase.entities.fs.image:image-id image)))
          (vase.entities.fs.image.repository:delete-bulk db (list image-id))
          (vase.entities.fs.image.repository:add-bulk db (list image))
          (vase.entities.folder:make-thumbnail/image image-id))))))

(defun add-dir-contents (db dir &key id-generator)
  (let ((images (create-images-bulk id-generator (dir-image-paths dir))))
    (let ((image-ids (mapcar #'vase.entities.fs.image:image-id images)))
      (vase.entities.fs.image.repository:delete-bulk db image-ids)
      (vase.entities.fs.image.repository:add-bulk db images)
      (mapcar #'vase.entities.folder.content:make-content/image image-ids))))


(defun add-bulk (db dirs &key id-generator make-thumbnail-file-fn)
  (let* ((folder-id-list
          (mapcar (lambda (dir)
                    (vase.entities.id:gen id-generator (dir-path dir)))
                  dirs))
         (folder-thumbnail-list
          (mapcar (lambda (dir)
                    (add-dir-thumbnail db dir
                     :id-generator id-generator
                     :make-thumbnail-file-fn make-thumbnail-file-fn))
                  dirs))
         (folders
          (mapcar (lambda (dir folder-id thumbnail)
                    (vase.entities.folder:make-folder
                     :id folder-id
                     :name (dir-path dir)
                     :thumbnail thumbnail
                     :modified-at (dir-modified-at dir)))
                  dirs folder-id-list folder-thumbnail-list)))
    (-> db
        ;; Delete existing folders if any
        (delete-bulk folder-id-list)
        ;; Save folders
        (vase.entities.folder.repository:save-bulk folders))
    (let* ((folder-contents-list
            (mapcar (lambda (dir)
                      (add-dir-contents db dir :id-generator id-generator))
                    dirs))
           (appendings
            (mapcar (lambda (folder contents)
                      (vase.entities.folder.content.op:make-appending
                       :folder folder :contents contents))
                    folders folder-contents-list))
           (appending-bulk
            (vase.entities.folder.content.op:make-appending-bulk
             :appendings appendings)))
      ;; Save contents
      (vase.entities.folder.content.repository:update db appending-bulk))))
(export 'add-bulk)

#+nil
(defun add (db name &key thumbnail id-generator)
  (let ((folder (vase.entities.folder:make-folder
                 :id (vase.entities.id:gen id-generator name)
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
    (vase.entities.folder.repository:save-bulk db (list folder))
    (folder->resp folder)))
