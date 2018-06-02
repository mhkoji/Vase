(defpackage :cocoa.testing.sqlite3
  (:use :cl))
(in-package :cocoa.testing.sqlite3)
(cl-annot:enable-annot-syntax)

@export
(defmacro with-sqlite3-dao ((dao) &rest body)
  `(proton:call/connection
    (make-instance 'proton:in-memory-sqlite3-factory)
    (lambda (conn)
      (let ((,dao (make-instance 'cocoa.infra.db.sqlite3:sqlite3-dao
                                 :connection conn)))
        (cocoa.infra.db.sqlite3:create-tables ,dao)
        ,@body))))
