(defpackage :cocoa.use-case.image
  (:use :cl :cocoa.entity.image))
(in-package :cocoa.use-case.image)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (image-id image) :path (image-path image)))

@export
(defun add-images (paths &key image-factory image-repository)
  (let ((images (make-images/paths image-factory paths)))
    (save-images image-repository images)
    (mapcar #'image->dto images)))

@export
(defun path/id (id &key image-repository)
  (let ((image (car (list-images/ids image-repository
                                     (list id)))))
    (when image
      (image-path image))))
