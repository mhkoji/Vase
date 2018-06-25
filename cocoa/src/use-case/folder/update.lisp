(defpackage :cocoa.use-case.folder.update
  (:use :cl))
(in-package :cocoa.use-case.folder.update)
(cl-annot:enable-annot-syntax)

@export
(defun change-thumbnail (folder-dao &key folder-id image-id)
  (let ((folder (car (cocoa.entity.folder:list-by-ids
                      folder-dao
                      (list folder-id)))))
    (setf (cocoa.entity.folder:folder-thumbnail folder)
          (cocoa.use-case.folder.thumbnail:make-of-image image-id))
    (cocoa.entity.folder:update folder-dao folder)))


@export
(defun append-contents (folder-dao &key folder-id contents)
  (let ((appending (cocoa.entity.folder:make-appending
                    :folder-id folder-id :contents contents)))
    (cocoa.entity.folder:update-contents folder-dao appending)))

@export
(defun delete-by-id (folder-id &key folder-dao)
  (cocoa.entity.folder:delete-by-ids folder-dao (list folder-id)))
