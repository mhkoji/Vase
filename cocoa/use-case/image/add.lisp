(defpackage :cocoa.use-case.image.add
  (:use :cl))
(in-package :cocoa.use-case.image.add)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (cocoa.fs.image:image-id image)
        :path (cocoa.fs.image:image-path image)))

@export
(defun prepare (image-repository path->image-id)
  "The use case of adding images"
  (lambda (paths)
    (let ((image-ids (mapcar path->image-id paths)))
      (let ((images (mapcar #'cocoa.fs.image:make-image image-ids paths)))
        (cocoa.fs.image:save-images image-repository images)
        (mapcar #'image->dto images)))))


@export
(defun exec (prepare paths)
  (funcall prepare paths))
