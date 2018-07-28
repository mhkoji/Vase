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

#+nil
(def-tests
    cocoa.test.scenario.folder:get-the-added-folder
    cocoa.test.scenario.folder:get-the-added-folder-images
    cocoa.test.scenario.folder:attach-tags-to-a-folder
    cocoa.test.scenario.folder:change-tags-attached-a-folder
    cocoa.test.scenario.folder:list-the-overviews-of-added-folders
    cocoa.test.scenario.tag:change-the-name-of-a-tag
    cocoa.test.scenario.tag:delete-a-tag)


(TEST TEST-SCENARIO
  (WITH-SQLITE3-DB (DB)
    (COCOA.TEST.SCENARIO.FOLDER:GET-THE-ADDED-FOLDER DB :TEST
                                                     IS))
  (WITH-SQLITE3-DB (DB)
    (COCOA.TEST.SCENARIO.FOLDER:GET-THE-ADDED-FOLDER-IMAGES DB
                                                            :TEST
                                                             IS))
  (WITH-SQLITE3-DB (DB)
    (COCOA.TEST.SCENARIO.FOLDER:ATTACH-TAGS-TO-A-FOLDER DB
                                                        :TEST
                                                        IS))
  (WITH-SQLITE3-DB (DB)
    (COCOA.TEST.SCENARIO.FOLDER:CHANGE-TAGS-ATTACHED-A-FOLDER
     DB :TEST IS))

  (WITH-SQLITE3-DB (DB)
    (COCOA.TEST.SCENARIO.FOLDER:LIST-THE-OVERVIEWS-OF-ADDED-FOLDERS
     DB :TEST IS))

  (WITH-SQLITE3-DB (DB)
    (COCOA.TEST.SCENARIO.TAG:CHANGE-THE-NAME-OF-A-TAG DB :TEST
                                                      IS))

  (WITH-SQLITE3-DB (DB) (COCOA.TEST.SCENARIO.TAG:DELETE-A-TAG DB :TEST IS)))
