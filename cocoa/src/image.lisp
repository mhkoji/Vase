(defpackage :cocoa.image
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.image)
(cl-annot:enable-annot-syntax)

@export
(defun get-path (id &key image-repository)
  "The use case of "
  (when-let ((image (car (cocoa.entity.fs.image:load-images-by-ids
                          image-repository
                          (list id)))))
    (cocoa.entity.fs.image:image-path image)))
