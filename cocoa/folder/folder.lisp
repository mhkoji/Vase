(defpackage :cocoa.folder
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

;;; The representation of a folder
@export
(defclass folder ()
  ((id :initarg :id)
   (name :initarg :name)
   (thumbnail :initarg :thumbnail)))

@export
(defun folder-id (folder)
  "Returns the unique id of a content"
  (slot-value folder 'id))

@export
(defun folder-name (folder)
  "Returns the name of a folder"
  (slot-value folder 'name))

@export
(defun folder-thumbnail (folder)
  "Returns the thumbanil of a folder"
  (slot-value folder 'thumbnail))

(defun (setf folder-thumbnail) (thumbnail folder)
  (setf (slot-value folder 'thumbnail) thumbnail))

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
