(defpackage :vase.tag.contents
  (:use :cl)
  (:export :content-id
           :content-type
           :bulk-load))
(in-package :vase.tag.contents)

(defgeneric content-id (content))

(defgeneric content-type (content))

(defgeneric bulk-load (repos type content-ids))
