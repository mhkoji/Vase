(defpackage :cocoa.use-case.folder.spec-fiveam
  (:use :cl :fiveam
        :cocoa.testing.sqlite3
        :cocoa.use-case.folder.spec))
(in-package :cocoa.use-case.folder.spec-fiveam)
(in-suite* :cocoa.use-case.folder :in :cocoa)

(test can-list-the-added-folders
  (with-sqlite3-dao (dao)
    (can-list-the-added-folders dao :test is)))
