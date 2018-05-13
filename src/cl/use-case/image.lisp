(defpackage :cocoa.use-case.image
  (:use :cl :cocoa.entity.image))
(in-package :cocoa.use-case.image)
(cl-annot:enable-annot-syntax)

@export
(defun path/id (id &key image-repository)
  (let ((image (car (list-images/ids image-repository
                                     (list id)))))
    (when image
      (image-path image))))
