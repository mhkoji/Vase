(defpackage :cocoa.ext.test.use-case.folder-fiveam
  (:use :cl :fiveam
        :cocoa.ext.testing.sqlite3
        :cocoa.use-case.folder.spec))
(in-package :cocoa.ext.test.use-case.folder-fiveam)
(in-suite* :cocoa.ext.test.use-case.folder :in :cocoa)

(test can-list-the-added-folders
  (with-sqlite3-dao (dao)
    (can-list-the-added-folders dao :test is)))
