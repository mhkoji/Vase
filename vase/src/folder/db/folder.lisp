(defpackage :vase.folder.db.folder
  (:use :cl)
  (:shadow :delete)
  (:export :make-row
           :row-folder-id
           :row-name
           :row-modified-at
           :select
           :select-ids
           :search-ids
           :insert
           :delete))
(in-package :vase.folder.db.folder)

;;; A primitive language to access the db
(defstruct row folder-id name modified-at)
(defgeneric select (db ids))
(defgeneric select-ids (db offset size))
(defgeneric search-ids (db keyword))
(defgeneric insert (db rows))
(defgeneric delete (db ids))
