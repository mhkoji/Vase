(defpackage :vase.app.image
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :vase.app.image)
(cl-annot:enable-annot-syntax)

@export
(defun get-path (db id)
  "The use case of "
  (when-let ((image (car (vase.entities.fs.image.repository:load-by-ids db
                          (list id)))))
    (vase.entities.fs.image:image-path image)))
