(defpackage :vase.entities.folder
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :vase.entities.folder)
(cl-annot:enable-annot-syntax)

;;; A representation of a folder
(defstruct folder id name thumbnail modified-at)
(export '(make-folder
          folder-id
          folder-name
          folder-thumbnail
          folder-modified-at))

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
