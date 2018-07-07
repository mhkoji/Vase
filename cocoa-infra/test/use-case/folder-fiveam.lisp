(defpackage :cocoa.infra.test.use-case.folder-fiveam
  (:use :cl :fiveam
        :cocoa.infra.testing.sqlite3
        :cocoa.use-case.folder.spec))
(in-package :cocoa.infra.test.use-case.folder-fiveam)
(in-suite* :cocoa.infra.test.use-case.folder :in :cocoa)

(test can-list-the-added-folders
  (with-sqlite3-dao (dao)
    (can-list-the-added-folders dao :test is)))

(test can-attach-tags-to-a-folder
  (with-sqlite3-dao (dao)
    (can-attach-tags-to-a-folder dao :test is)))
