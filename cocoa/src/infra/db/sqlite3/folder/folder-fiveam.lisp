(defpackage :cocoa.infra.db.sqlite3.folder.folder-fiveam
  (:use :cl :fiveam
        :cocoa.entity.folder
        :cocoa.entity.folder.folder-spec))
(in-package :cocoa.infra.db.sqlite3.folder.folder-fiveam)
(in-suite* :cocoa)

(defmacro with-sqlite3-dao ((dao) &rest body)
  `(proton:call/connection
    (make-instance 'proton:in-memory-sqlite3-factory)
    (lambda (conn)
      (let ((,dao (make-instance 'cocoa.infra.db.sqlite3:sqlite3-dao
                                 :connection conn)))
        (cocoa.infra.db.sqlite3:create-tables ,dao)
        ,@body))))

(test insert-then-can-select-the-inserted-rows
  (with-sqlite3-dao (dao)
    (is (can-insert-then-select-the-inserted-rows dao))))

(test insert-then-can-delete-the-inserted-rows
  (with-sqlite3-dao (dao)
    (is (can-insert-then-delete-the-inserted-rows dao))))
