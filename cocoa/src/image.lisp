(defpackage :cocoa.image
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.image)
(cl-annot:enable-annot-syntax)

@export
(defun get-path (db id)
  "The use case of "
  (when-let ((image (car (cocoa.entity.fs.image.repository:load-by-ids db
                          (list id)))))
    (cocoa.entity.fs.image:image-path image)))
