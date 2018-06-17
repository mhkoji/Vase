(defpackage :cocoa.use-case.image.add
  (:use :cl))
(in-package :cocoa.use-case.image.add)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (cocoa.entity.fs.image:image-id image)
        :path (cocoa.entity.fs.image:image-path image)))

(defstruct executor image-factory image-dao)
(export 'make-executor)

@export
(defun execute (executor paths)
  (let ((images (cocoa.entity.fs.image:make-by-paths
                 (executor-image-factory executor)
                 paths)))
    (cocoa.entity.fs.image:save (executor-image-dao executor) images)
    (mapcar #'image->dto images)))
