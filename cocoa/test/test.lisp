(defpackage :cocoa.db.test
  (:use :cl :fiveam
        :cocoa.db.testing.sqlite3))
(in-package :cocoa.db.test)
(in-suite* :cocoa.db.test :in :cocoa)

(test test-entities
  (with-sqlite3-dao (dao)
    (cocoa.entity.folder-spec:can-insert-then-select-the-inserted-rows
     dao :test is))

  (with-sqlite3-dao (dao)
    (is (cocoa.entity.folder-spec:can-insert-then-delete-the-inserted-rows
         dao)))

  (with-sqlite3-dao (dao)
    (is (cocoa.entity.folder-spec:folder-can-contain-contents
         (cocoa.entity.folder:folder-repository dao)))))

(test test-applications
  (with-sqlite3-dao (dao)
    (cocoa.folder.folder-spec:can-get-the-added-folder dao :test is))

  (with-sqlite3-dao (dao)
    (cocoa.folder.folder-spec:can-attach-tags-to-a-folder dao :test is)))

