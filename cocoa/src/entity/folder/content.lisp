;;; The representation of each content in a folder
(defpackage :cocoa.entity.folder.content
  (:use :cl))
(in-package :cocoa.entity.folder.content)
(cl-annot:enable-annot-syntax)

@export
(defgeneric content-id (content)
  (:documentation "Returns the unique id of a content"))

@export
(defgeneric add-by-folder-id (folder-repository folder-id contents))

@export
(defgeneric list-by-folder-id (folder-repository folder-id))
