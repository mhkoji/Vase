(defpackage :vase.db.folder.thumbnail
  (:use :cl)
  (:shadow :delete)
  (:export :make-row
           :row-folder-id
           :row-thumbnail-id
           :select
           :insert
           :delete))
(in-package :vase.db.folder.thumbnail)

(defgeneric make-row (db folder-id thumbnail-id))
(defgeneric row-folder-id (thumbnail-row))
(defgeneric row-thumbnail-id (thumbnail-row))
(defgeneric select (db folder-id-list))
(defgeneric insert (db thumbnail-row-list))
(defgeneric delete (db folder-id-list))
