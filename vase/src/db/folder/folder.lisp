(defpackage :vase.db.folder
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
(in-package :vase.db.folder)

;;; A primitive language to access the db
(defgeneric make-row (db folder-id name modified-at))
(defgeneric row-folder-id (folder-row))
(defgeneric row-name (folder-row))
(defgeneric row-modified-at (folder-row))
(defgeneric select (db ids))
(defgeneric select-ids (db offset size))
(defgeneric search-ids (db keyword))
(defgeneric insert (db folder-rows))
(defgeneric delete (db folder-id-list))
