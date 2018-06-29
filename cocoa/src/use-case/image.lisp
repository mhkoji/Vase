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
(defun add-images (image-factory image-dao)
  "The use case of adding images"
  (lambda (paths)
    (let ((images (cocoa.entity.fs.image:make-by-paths image-factory
                                                       paths)))
      (cocoa.entity.fs.image:save image-dao images)
      (mapcar #'image->dto images))))

@export
(defun get-path (image-dao)
  "The use case of "
  (lambda (id)
    (when-let ((image (car (cocoa.entity.fs.image:list-by-ids
                            image-dao
                            (list id)))))
      (cocoa.entity.fs.image:image-path image))))
