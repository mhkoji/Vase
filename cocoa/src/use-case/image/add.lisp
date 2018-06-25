(defpackage :cocoa.use-case.image.add
  (:use :cl))
(in-package :cocoa.use-case.image.add)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (cocoa.entity.fs.image:image-id image)
        :path (cocoa.entity.fs.image:image-path image)))

(defstruct add-images image-factory image-dao)
(export 'make-add-images)

@export
(defun call (add-images paths)
  "The use case of adding images"
  (let ((images (cocoa.entity.fs.image:make-by-paths
                 (add-images-image-factory add-images)
                 paths)))
    (cocoa.entity.fs.image:save (add-images-image-dao add-images)
                                images)
    (mapcar #'image->dto images)))
