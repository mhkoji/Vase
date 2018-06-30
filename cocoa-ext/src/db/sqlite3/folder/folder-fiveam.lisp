(defpackage :cocoa.ext.db.sqlite3.folder.folder-fiveam
  (:use :cl :fiveam
        :cocoa.folder
        :cocoa.folder.folder-spec
        :cocoa.testing.sqlite3))
(in-package :cocoa.ext.db.sqlite3.folder.folder-fiveam)
(in-suite* :cocoa.ext.db.sqlite3.folder :in :cocoa)

(test insert-then-can-select-the-inserted-rows
  (with-sqlite3-dao (dao)
    (can-insert-then-select-the-inserted-rows dao :test is)))

(test insert-then-can-delete-the-inserted-rows
  (with-sqlite3-dao (dao)
    (is (can-insert-then-delete-the-inserted-rows dao))))

(test folder-can-contain-contents
  (with-sqlite3-dao (dao)
    (is (folder-can-contain-contents (folder-repository dao)))))
