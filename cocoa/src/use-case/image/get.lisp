(defpackage :cocoa.use-case.image.get
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.use-case.image.get)
(cl-annot:enable-annot-syntax)

(defstruct executor image-dao)
(export 'make-executor)

@export
(defun path (executor id)
  (when-let ((image (car (cocoa.entity.fs.image:list-by-ids
                          (executor-image-dao executor)
                          (list id)))))
    (cocoa.entity.fs.image:image-path image)))
