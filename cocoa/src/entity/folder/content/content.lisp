(defpackage :cocoa.entity.folder.content
  (:use :cl))
(in-package :cocoa.entity.folder.content)
(cl-annot:enable-annot-syntax)

;;;; A representation of a content in a folder
@export
(defgeneric content-id (content)
  (:documentation "Returns the unique id of a content"))

@export
(defclass content ()
  ((id :initarg :id :reader content-id)))

@export
(defun make-content (id)
  (make-instance 'content :id id))

@export
(defun make-content/image (image-id)
  "Create a content from the given id. It is a caller's responsiblity to give a valid id"
  (make-content (format nil "image:~A" image-id)))

@export
(defun content->image-id (content)
  (cl-ppcre:register-groups-bind (image-id)
      ("image:(.*)" (content-id content))
    image-id))
