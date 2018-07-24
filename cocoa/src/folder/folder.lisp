(defpackage :cocoa.folder
  (:use :cl)
  (:import-from :cl-arrows :-> :->> :-<>))
(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

(defun thumbnail->resp (thumbnail)
  (list :id (cocoa.entity.folder:thumbnail->image-id thumbnail)))

(defun folder->resp (folder)
  (list :id (cocoa.entity.folder:folder-id folder)
        :name (cocoa.entity.folder:folder-name folder)
        :thumbnail (thumbnail->resp (cocoa.entity.folder:folder-thumbnail
                                     folder))))
;;;; Get
@export
(defun get-folder (id &key db)
  (cocoa.folder.util:accept-folder-id id)
  (->> (car (cocoa.entity.folder.repository:load-by-ids db (list id)))
       folder->resp))


;;;; Update
@export
(defun update-thumbnail (folder-id image-id &key db)
  (cocoa.folder.util:accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder.repository:load-by-ids
                      db
                      (list folder-id)))))
    (setf (cocoa.entity.folder:folder-thumbnail folder)
          (cocoa.entity.folder:make-thumbnail/image image-id))
    (cocoa.entity.folder.repository:update db folder)))


;;;; Delete
@export
(defun delete-folder (folder-id &key db)
  (cocoa.folder.util:accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder.repository:load-by-ids
                      db
                      (list folder-id)))))
    (let ((deleting-bulk
           (cocoa.entity.folder.content.repository:make-deleting-bulk
            (list folder))))
      (cocoa.entity.folder.content.repository:update db deleting-bulk)))
  (cocoa.entity.folder.repository:delete-by-ids db (list folder-id)))

;;;; Add
;;; The representation of a directory in the local file system
(defstruct dir path image-paths modified-at)
(export 'make-dir)

(defun dir-thumbnail (dir &key make-thumbnail-file
                               id-generator
                               image-repository)
  (let ((thumbnail-path (funcall make-thumbnail-file
                                 (car (dir-image-paths dir)))))
    (let ((image-id (cocoa.entity.id:gen id-generator thumbnail-path)))
      (cocoa.entity.fs.image:save-images
       image-repository
       (list (cocoa.entity.fs.image:make-image
              image-id
              thumbnail-path)))
      (cocoa.entity.folder:make-thumbnail/image image-id))))

(defun dir-contents (dir &key id-generator image-repository)
  (let ((image-ids (mapcar (alexandria:curry #'cocoa.entity.id:gen
                                             id-generator)
                           (dir-image-paths dir))))
    (cocoa.entity.fs.image:save-images
     image-repository
     (mapcar #'cocoa.entity.fs.image:make-image
             image-ids
             (dir-image-paths dir)))
    (mapcar #'cocoa.entity.folder.content:make-content/image image-ids)))

@export
(defun add-bulk (dirs &key id-generator
                           image-repository
                           db
                           folder-content-repository
                           make-thumbnail-file)
  (labels ((dir-folder (dir)
             (cocoa.entity.folder:make-folder
              :id (cocoa.entity.id:gen id-generator (dir-path dir))
              :name (dir-path dir)
              :thumbnail (dir-thumbnail dir
                          :make-thumbnail-file make-thumbnail-file
                          :id-generator id-generator
                          :image-repository image-repository)
              :modified-at (dir-modified-at dir))))
    (let ((folders (mapcar #'dir-folder dirs)))
      (cocoa.entity.folder.repository:save-all db folders)
      (let ((deleting-bulk
             (cocoa.entity.folder.content.op:make-deleting-bulk
              :folders folders)))
        (cocoa.entity.folder.content.repository:update db deleting-bulk))
      (let ((appendings
             (loop for dir in dirs
                   for folder in folders
                   for contents = (dir-contents dir
                                   :id-generator id-generator
                                   :image-repository image-repository)
                   collect (cocoa.entity.folder.content.op:make-appending
                            :folder folder
                            :contents contents))))
        (let ((appending-bulk
               (cocoa.entity.folder.content.op:make-appending-bulk
                :appendings appendings)))
          (cocoa.entity.folder.content.repository:update
           db appending-bulk)))
      (mapcar #'folder->resp folders))))

(defun add (&key name thumbnail
                 db
                 id-generator)
  (let ((folder (cocoa.entity.folder:make-folder
                 :id (cocoa.entity.id:gen id-generator name)
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
    (cocoa.entity.folder.repository:save-all db (list folder))
    (folder->resp folder)))
