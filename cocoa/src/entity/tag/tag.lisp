(defpackage :cocoa.entity.tag
  (:use :cl))
(in-package :cocoa.entity.tag)
(cl-annot:enable-annot-syntax)

;;; Content, to which tags are attached
@export
(defgeneric content-id (content))
@export
(defgeneric content-type (content))

(defclass simple-content ()
  ((id
    :initarg :id
    :reader content-id)
   (type
    :initarg :type
    :type :keyword
    :reader content-type)))

@export
(defun make-simple-content (id type)
  (make-instance 'simple-content :id id :type type))

;;; Tag
@export
(defgeneric tag-id (tag))
@export
(defgeneric tag-name (tag))
@export
(defgeneric tag-contents (tag))

@export
(defgeneric attach-tag (tag content))
@export
(defgeneric detach-tag (tag content))

@export
(defgeneric make-tag/name (factory name))

@export
(defgeneric save-tag (repository tag))
@export
(defgeneric list-tags/range (repository offset size))
@export
(defgeneric list-tags/ids (repository ids))
@export
(defgeneric list-tags/content (repository content))
@export
(defgeneric delete-tags/ids (repository ids))
