(defpackage :cocoa.db.testing.sqlite3
  (:use :cl))
(in-package :cocoa.db.testing.sqlite3)
(cl-annot:enable-annot-syntax)

@export
(defmacro with-sqlite3-db ((db) &rest body)
  `(proton:call/connection
    (make-instance 'proton:in-memory-sqlite3-factory)
    (lambda (conn)
      (let ((,db (make-instance 'cocoa.db.sqlite3:sqlite3-db
                                 :connection conn)))
        (cocoa.db.sqlite3:create-tables ,db)
        ,@body))))
