(defpackage :vase.folder.content
  (:use :cl)
  (:export :content-type
           :content-entity-id

           :bulk-load))
(in-package :vase.folder.content)

(defgeneric content-type (c))

(defgeneric content-entity-id (c))

(defgeneric bulk-load (repos type entity-ids))
