(defpackage :vase.db
  (:use :cl)
  (:export :initialize
           :connection->db))
(in-package :vase.db)

(defgeneric initialize (db))

(defgeneric connection->db (conn))
