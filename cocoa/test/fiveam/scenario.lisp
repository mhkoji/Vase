(defpackage :cocoa.test.fiveam.scenario
  (:use :cl :fiveam
        :cocoa.test.testing.sqlite3))
(in-package :cocoa.test.fiveam.scenario)
(in-suite :cocoa)

(defmacro def-tests (&rest symbols)
  `(progn
     ,@(mapcar (lambda (symbol)
                 `(test ,(intern (symbol-name symbol))
                    (with-sqlite3-db (db)
                      (,symbol db :test is))))
               symbols)))

(def-tests
    cocoa.test.scenario.folder:get-the-added-folder
    cocoa.test.scenario.folder:get-the-added-folder-images
    cocoa.test.scenario.folder:attach-tags-to-a-folder
    cocoa.test.scenario.folder:change-tags-attached-a-folder
    cocoa.test.scenario.folder:list-the-overviews-of-added-folders
    cocoa.test.scenario.tag:change-the-name-of-a-tag
    cocoa.test.scenario.tag:delete-a-tag)
