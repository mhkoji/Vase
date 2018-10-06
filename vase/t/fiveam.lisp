(defpackage :vase.t.fiveam
  (:use :cl :fiveam
        :vase.t.testing.sqlite3))
(in-package :vase.t.fiveam)

(defmacro def-tests (&rest symbols)
  `(progn
     ,@(mapcar (lambda (symbol)
                 `(test ,(intern (symbol-name symbol))
                    (with-sqlite3-db (db)
                      (,symbol db :test is))))
               symbols)))

(def-suite :vase)
(in-suite* :vase)

;(in-suite* :cocoa.entities.folder :in :cocoa)
(def-tests
    vase.t.scenario.entities.folder.db:insert-then-select-the-inserted-rows
    vase.t.scenario.entities.folder.db:insert-then-delete-the-inserted-rows
    vase.t.scenario.entities.folder:contains-contents
    )

;(in-suite* :cocoa.folder :in :cocoa)
(def-tests
    vase.t.scenario.folder:get-the-added-folder
    vase.t.scenario.folder:get-the-added-folder-images
    vase.t.scenario.folder:attach-tags-to-a-folder
    vase.t.scenario.folder:change-tags-attached-a-folder
    vase.t.scenario.folder:list-the-overviews-of-added-folders
    )

;(in-suite* :cocoa.tag :in :cocoa)
(def-tests
    vase.t.scenario.tag:change-the-name-of-a-tag
    vase.t.scenario.tag:delete-a-tag
    )
