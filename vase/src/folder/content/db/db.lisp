(defpackage :vase.folder.content.db
  (:use :cl)
  (:shadow :delete)
  (:export :select-content-ids
           :insert
           :delete))
(in-package :vase.folder.content.db)

(defgeneric select-content-ids (db folder-id))
(defgeneric insert (db folder-id content-ids))
(defgeneric delete (db folder-ids))
