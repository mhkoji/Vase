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

(in-suite* :vase)

(defmethod vase.id:gen ((generator function) (string string))
  (funcall generator string))

(defmethod vase.folder.thumbnail:bulk-load ((f function) thumbnail-ids)
  (funcall f thumbnail-ids))


(def-tests
    vase.t.scenario.folder.repos.db:insert-then-select-the-inserted-rows
    vase.t.scenario.folder.repos.db:insert-then-delete-the-inserted-rows
    vase.t.scenario.folder.content.repos:append-contents-then-bulk-load
    )

(def-tests
    vase.t.scenario.folder:load-the-added-folder
    )

(def-tests
    vase.t.scenario.tag:change-the-name-of-a-tag
    vase.t.scenario.tag:delete-a-tag
    vase.t.scenario.tag:attach-tags-to-a-folder
    vase.t.scenario.tag:change-tags-attached-a-folder
    )
