(defpackage :vase.image.db
  (:use :cl)
  (:shadow :delete)
  (:export :make-row
           :row-path
           :row-image-id
           :insert
           :select
           :delete))
(in-package :vase.image.db)

(defstruct row image-id path)
(defgeneric insert (db images))
(defgeneric select (db ids))
(defgeneric delete (db ids))
