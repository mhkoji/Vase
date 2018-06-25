(defpackage :cocoa.use-case.image.get-path
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.use-case.image.get-path)
(cl-annot:enable-annot-syntax)

@export
(defun call (image-dao id)
  (when-let ((image (car (cocoa.entity.fs.image:list-by-ids
                          image-dao
                          (list id)))))
    (cocoa.entity.fs.image:image-path image)))
