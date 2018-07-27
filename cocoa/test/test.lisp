(defpackage :cocoa.db.test
  (:use :cl :fiveam
        :cocoa.db.testing.sqlite3))
(in-package :cocoa.db.test)
(in-suite* :cocoa.db.test :in :cocoa)

(test test-entities
  (with-sqlite3-db (db)
    (cocoa.entity.folder.db-spec:can-insert-then-select-the-inserted-rows
     db :test is))

  (with-sqlite3-db (db)
    (is (cocoa.entity.folder.db-spec:can-insert-then-delete-the-inserted-rows
         db)))

  (with-sqlite3-db (db)
    (is (cocoa.entity.folder.folder-spec:folder-can-contain-contents db)))

  )

(test test-applications
  (with-sqlite3-db (db)
    (cocoa.folder.folder-spec:can-get-the-added-folder
     db :test is))

  (with-sqlite3-db (db)
    (cocoa.folder.folder-spec:can-get-the-added-folder-images
     db :test is))

  (with-sqlite3-db (db)
    (cocoa.folder.folder-spec:can-attach-tags-to-a-folder
     db :test is))

  (with-sqlite3-db (db)
    (cocoa.folder.folder-spec:can-list-the-overviews-of-added-folders
     db :test is))
  )

