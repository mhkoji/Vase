(defpackage :cocoa.use-case.image
  (:use :cl :cocoa.entity.fs.image))
(in-package :cocoa.use-case.image)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (image-id image) :path (image-path image)))

@export
(defclass registry ()
  ((image-factory
    :reader image-factory
    :initarg image-factory)
   (image-repository
    :reader image-repository
    :initarg image-repository)))

@export
(defgeneric add-by-paths (registry paths))
@export
(defgeneric get-by-id (registry image-id))


(defmethod add-by-paths ((registry registry) paths)
  (with-accessors ((image-factory image-factory)
                   (image-repository image-repository)) registry
    (let ((images (make-images/paths image-factory paths)))
      (save-images image-repository images)
      (mapcar #'image->dto images))))

(defmethod get-path-by-id ((registry registry) (image-id string))
  (with-accessors ((image-repository image-repository)) registry
    (let ((image (car (list-images/ids image-repository (list id)))))
      (when image
        (image-path image)))))
