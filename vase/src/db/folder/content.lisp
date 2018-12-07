(defpackage :vase.db.folder.content
  (:use :cl)
  (:shadow :delete)
  (:export :select-content-ids
           :insert
           :delete))
(in-package :vase.db.folder.content)

(defgeneric select-content-ids (db folder-id))
(defgeneric insert (db folder-id content-ids))
(defgeneric delete (db folder-ids))
