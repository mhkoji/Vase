;;; The thumbnail of a folder
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

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
