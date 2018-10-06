(in-package :vase.db.sqlite3.folder)

(defmethod folder-content-insert ((db sqlite3-db)
                                  (folder-id string)
                                  (content-id-list list))
  (insert-bulk db +folder-contents+ (list +folder-id+ +content-id+)
   (mapcar (lambda (content-id) (list folder-id content-id))
           content-id-list))
  db)

(defmethod folder-content-select-ids ((db sqlite3-db) folder-id)
  (let ((q (join " SELECT"
                 " " +content-id+
                 " FROM"
                 " " +folder-contents+
                 " WHERE"
                 " " +folder-id+ " = ?")))
    (mapcar #'second (query db q (list folder-id)))))

(defmethod folder-content-delete ((db sqlite3-db) folder-ids)
  (delete-bulk db +folder-contents+ +folder-id+ folder-ids)
  db)
