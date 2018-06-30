(defpackage :cocoa.tag
  (:use :cl)
  (:import-from :cl-arrows :->> :->))
(in-package :cocoa.tag)
(cl-annot:enable-annot-syntax)

;;;; The definition of a content, to which tags are attached
@export
(defclass content ()
  ((id
    :initarg :id
    :reader content-id)
   (type
    :initarg :type
    :type :keyword
    :reader content-type)))
(export '(content-id content-type))

;;;; The definition of a tag
;;;; Generic functions are used becase some persistent system for the jobs of a tag should be hidden.
@export
(defgeneric tag-id (tag))
@export
(defgeneric tag-name (tag))
@export
(defgeneric tag-contents (tag))
