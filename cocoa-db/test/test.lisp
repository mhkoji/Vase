(defpackage :cocoa.db.test
  (:use :cl :fiveam
        :cocoa.db.testing.sqlite3))
(in-package :cocoa.db.test)
(in-suite* :cocoa.db.test :in :cocoa)

(test test-all
  ;; cocoa.folder
  (with-sqlite3-dao (dao)
    (cocoa.folder-spec:can-insert-then-select-the-inserted-rows
     dao :test is))

  (with-sqlite3-dao (dao)
    (is (cocoa.folder-spec:can-insert-then-delete-the-inserted-rows dao)))

  (with-sqlite3-dao (dao)
    (is (cocoa.folder-spec:folder-can-contain-contents
         (cocoa.folder:folder-repository dao))))

  ;; cooca.spec
  (with-sqlite3-dao (dao)
    (cocoa.spec.folder:can-get-the-added-folder dao :test is))

  (with-sqlite3-dao (dao)
    (cocoa.spec.folder:can-attach-tags-to-a-folder dao :test is)))
