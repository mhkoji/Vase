(defpackage :vase.folder.content
  (:use :cl)
  (:export :content-type
           :content-entity-id

           :bulk-delete
           :make-appending
           :bulk-append

           :bulk-load
           :bulk-load-by-folder))
(in-package :vase.folder.content)

(defgeneric content-type (c))

(defgeneric content-entity-id (c))

(defgeneric bulk-load (repos type entity-ids))
