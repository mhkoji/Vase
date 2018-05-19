(defpackage :cocoa.use-case.folder.inject
  (:use :cl
        :cocoa.util.stream
        :cocoa.entity.folder))
(in-package :cocoa.use-case.folder.inject)
(cl-annot:enable-annot-syntax)

@export
(defun image->thumbnail (image)
  (make-simple-thumbnail (cocoa.entity.image:image-id image)))

@export
(defun thumbnail->image-id (thumbnail)
  (thumbnail-id thumbnail))

@export
(defun image->content (image)
  (make-simple-content
   (format nil "image:~A" (cocoa.entity.image:image-id image))))

@export
(defun content->image-id (content)
  (cl-ppcre:register-groups-bind (image-id)
      ("image:(.*)" (content-id content))
    image-id))
