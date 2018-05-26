(defpackage :cocoa.infra.db.folder.content.sqlite3
  (:use :cl
        :cocoa.entity.folder.content
        :cocoa.infra.db.folder.dao
        :cocoa.infra.db.sqlite3))
(in-package :cocoa.infra.db.folder.content.sqlite3)

(defmethod folder-content-insert ((dao sqlite3-dao)
                                  (folder-id string)
                                  (content-id-list list))
  (insert-bulk dao +folder-contents+ (list +folder-id+ +content-id+)
   (mapcar (lambda (content-id) (list folder-id content-id))
           content-id-list))
  dao)

(defmethod folder-content-select-ids ((dao sqlite3-dao) folder-id)
  (let ((q (join " SELECT"
                 " " +content-id+
                 " FROM"
                 " " +folder-contents+
                 " WHERE"
                 " " +folder-id+ " = ?")))
    (mapcar #'second (query dao q (list folder-id)))))

(defmethod folder-content-delete ((dao sqlite3-dao) folder-ids)
  (delete-bulk dao +folder-contents+ +folder-id+ folder-ids)
  dao)
