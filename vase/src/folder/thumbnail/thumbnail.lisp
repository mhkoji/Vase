(defpackage :vase.folder.thumbnail
  (:use :cl)
  (:export :thumbnail-id
           :bulk-load
           :bulk-delete))
(in-package :vase.folder.thumbnail)

(defgeneric thumbnail-id (th))

(defgeneric bulk-load (repos thumbnail-ids))

(defgeneric bulk-delete (repos thumbnail-ids))
