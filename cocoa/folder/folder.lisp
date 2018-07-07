(defpackage :cocoa.folder
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

;;; The representation of a folder
(defstruct folder id name thumbnail modified-at)
(export '(make-folder
          folder-id
          folder-name
          folder-thumbnail))

;;;; The representation of the thumbnail of a folder
@export
(defgeneric thumbnail-id (thumbnail)
  (:documentation "Returns the unique id of a thumbnail"))

@export
(defclass thumbnail ()
  ((id :initarg :id :reader thumbnail-id)))

@export
(defun make-thumbnail (id)
  "Create a thumbnail from the given id. It is a caller's responsiblity to give a valid id"
  (make-instance 'thumbnail :id id))


;;;; The representation of a content in a folder
@export
(defgeneric content-id (content)
  (:documentation "Returns the unique id of a content"))

@export
(defclass content ()
  ((id :initarg :id :reader content-id)))

@export
(defun make-content (id)
  "Create a content from the given id. It is a caller's responsiblity to give a valid id"
  (make-instance 'content :id id))
