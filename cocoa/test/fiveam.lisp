(defpackage :cocoa.test.fiveam
  (:use :cl :fiveam
        :cocoa.test.testing.sqlite3))
(in-package :cocoa.test.fiveam)

(defmacro def-tests (&rest symbols)
  `(progn
     ,@(mapcar (lambda (symbol)
                 `(test ,(intern (symbol-name symbol))
                    (with-sqlite3-db (db)
                      (,symbol db :test is))))
               symbols)))

(def-suite :cocoa)
(in-suite* :cocoa)

;(in-suite* :cocoa.entity.folder :in :cocoa)
(def-tests
    cocoa.test.scenario.entity.folder.db:insert-then-select-the-inserted-rows
    cocoa.test.scenario.entity.folder.db:insert-then-delete-the-inserted-rows
    cocoa.test.scenario.entity.folder:contains-contents
    )

;(in-suite* :cocoa.folder :in :cocoa)
(def-tests
    cocoa.test.scenario.folder:get-the-added-folder
    cocoa.test.scenario.folder:get-the-added-folder-images
    cocoa.test.scenario.folder:attach-tags-to-a-folder
    cocoa.test.scenario.folder:change-tags-attached-a-folder
    cocoa.test.scenario.folder:list-the-overviews-of-added-folders
    )

;(in-suite* :cocoa.tag :in :cocoa)
(def-tests
    cocoa.test.scenario.tag:change-the-name-of-a-tag
    cocoa.test.scenario.tag:delete-a-tag
    )
