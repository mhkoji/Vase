(defpackage :cocoa.test.fiveam.unit
  (:use :cl :fiveam
        :cocoa.test.testing.sqlite3))
(in-package :cocoa.test.fiveam.unit)
(in-suite :cocoa)

(test test-units
  (with-sqlite3-db (db)
    (cocoa.entity.folder.db-spec:can-insert-then-select-the-inserted-rows
     db :test is))

  (with-sqlite3-db (db)
    (is (cocoa.entity.folder.db-spec:can-insert-then-delete-the-inserted-rows
         db)))

  (with-sqlite3-db (db)
    (is (cocoa.entity.folder.folder-spec:folder-can-contain-contents db)))

  )
