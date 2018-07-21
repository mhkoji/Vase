(defpackage :cocoa.entity.folder
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

;;; A representation of a folder
(defstruct folder id name thumbnail modified-at)
(export '(make-folder
          folder-id
          folder-name
          folder-thumbnail))


;;;; A representation of the thumbnail of a folder
@export
(defgeneric thumbnail-id (thumbnail)
  (:documentation "Returns the unique id of a thumbnail"))

@export
(defclass thumbnail ()
  ((id :initarg :id :reader thumbnail-id)))

@export
(defun make-thumbnail (id)
  (make-instance 'thumbnail :id id))

@export
(defun make-thumbnail/image (image-id)
  "Make an image instance from the given image. It is a caller's responsiblity to give a valid id"
  (make-thumbnail image-id))

@export
(defun thumbnail->image-id (thumbnail)
  "Extract the image id of the thumbnail, if any"
  (thumbnail-id thumbnail))


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
