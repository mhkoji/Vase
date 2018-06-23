(defpackage :cocoa.entity.note
  (:use :cl))
(in-package :cocoa.entity.note)
(cl-annot:enable-annot-syntax)

@export
(defgeneric memo-id (memo))
@export
(defgeneric memo-type (memo))
@export
(defclass memo-value () ())


@export
(defgeneric note-id (note))
@export
(defgeneric note-get-value (note memo))
@export
(defgeneric note-set-value (note memo))
@export
(defgeneric note-rem-value (note memo))

@export
(defgeneric make-note (note))
@export
(defgeneric copy-note (note))

