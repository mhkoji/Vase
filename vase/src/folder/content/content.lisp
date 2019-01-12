(defpackage :vase.folder.content
  (:use :cl)
  (:export :folder-id
           :content-type
           :content-entity-id

           :bulk-delete
           :bulk-append
           :make-appending
           :bulk-load
           :bulk-load-by-folder))
(in-package :vase.folder.content)

(defgeneric folder-id (f))

(defgeneric content-type (c))

(defgeneric content-entity-id (c))

(defgeneric bulk-load (entity-repos type entity-ids))
