(defpackage :vase.folder.content.repos.sqlite3
  (:use :cl
        :vase.folder.content.repos.db
        :vase.db.sqlite3)
  (:shadowing-import-from :vase.folder.content.repos.db :delete))
(in-package :vase.folder.content.repos.sqlite3)

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
