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


(defstruct content-query folder-id from size)
(export 'make-content-query)
(export 'content-query)
(export 'content-query-folder-id)
(export 'content-query-from)
(export 'content-query-size)

@export
(defgeneric list-by-query (folder-repository content-query))
