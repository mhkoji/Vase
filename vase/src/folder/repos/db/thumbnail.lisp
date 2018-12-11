(defpackage :vase.folder.repos.db.thumbnail
  (:use :cl)
  (:shadow :delete)
  (:export :make-row
           :row-folder-id
           :row-thumbnail-id
           :select
           :insert
           :delete))
(in-package :vase.folder.repos.db.thumbnail)

(defstruct row folder-id thumbnail-id)
(defgeneric select (db folder-id-list))
(defgeneric insert (db thumbnail-row-list))
(defgeneric delete (db folder-id-list))
