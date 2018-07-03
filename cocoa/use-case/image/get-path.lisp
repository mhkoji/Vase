(defpackage :cocoa.use-case.image.get-path
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.use-case.image.get-path)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (image-repository)
  "The use case of "
  (lambda (id)
    (when-let ((image (car (cocoa.fs.image:load-images-by-ids
                            image-repository
                            (list id)))))
      (cocoa.fs.image:image-path image))))

@export
(defun exec (prepare id)
  (funcall prepare id))
