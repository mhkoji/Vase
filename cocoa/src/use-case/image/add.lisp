(defpackage :cocoa.use-case.image.add
  (:use :cl))
(in-package :cocoa.use-case.image.add)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (cocoa.entity.fs.image:image-id image)
        :path (cocoa.entity.fs.image:image-path image)))

@export
(defun make-add-images (&key image-factory image-dao)
  (lambda (paths)
    (let ((images (cocoa.entity.fs.image:make-by-paths image-factory
                                                       paths)))
      (cocoa.entity.fs.image:save image-dao images)
      (mapcar #'image->dto images))))

@export
(defun call (add-images paths)
  "The use case of adding images"
  (funcall add-images paths))

@export
(defun by-paths (add-images path)
  "The use case of adding images by paths"
  (funcall add-images paths))
