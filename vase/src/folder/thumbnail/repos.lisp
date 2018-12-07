(defpackage :vase.folder.thumbnail.repos
  (:use :cl)
  (:export :bulk-load
           :bulk-delete))
(in-package :vase.folder.thumbnail.repos)

(defgeneric bulk-load (repos thumbnail-ids))

(defgeneric bulk-delete (repos thumbnail-ids))
