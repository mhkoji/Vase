(defpackage :cocoa.use-case.image
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.use-case.image)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (cocoa.entity.fs.image:image-id image)
        :path (cocoa.entity.fs.image:image-path image)))

@export
(defun add-images (paths &key image-factory image-dao)
  (let ((images (cocoa.entity.fs.image:make-by-paths
                 image-factory
                 paths)))
    (cocoa.entity.fs.image:save image-dao images)
    (mapcar #'image->dto images)))

@export
(defun path/id (id &key image-dao)
  (when-let ((image (car (cocoa.entity.fs.image:list-by-ids
                          image-dao
                          (list id)))))
    (cocoa.entity.fs.image:image-path image)))
