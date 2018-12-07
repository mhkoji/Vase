(defpackage :vase.db.sqlite3.folder.content
  (:use :cl
        :vase.db.sqlite3
        :vase.db.folder.content)
  (:shadowing-import-from :vase.db.folder.content :delete)
  (:import-from :cl-arrows :->>))
(in-package :vase.db.sqlite3.folder.content)

(defmethod insert ((db sqlite3-db)
                   (folder-id string)
                   (content-id-list list))
  (insert-bulk db +folder-contents+ (list +folder-id+ +content-id+)
   (mapcar (lambda (content-id) (list folder-id content-id))
           content-id-list))
  db)

(defmethod select-content-ids ((db sqlite3-db) folder-id)
  (let ((q (join " SELECT"
                 " " +content-id+
                 " FROM"
                 " " +folder-contents+
                 " WHERE"
                 " " +folder-id+ " = ?")))
    (mapcar #'second (query db q (list folder-id)))))

(defmethod delete ((db sqlite3-db) folder-ids)
  (delete-bulk db +folder-contents+ +folder-id+ folder-ids)
  db)
