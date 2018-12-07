(defpackage :vase.db.sqlite3.folder
  (:use :cl
        :vase.db.sqlite3
        :vase.db.folder)
  (:shadowing-import-from :vase.db.folder :delete)
  (:import-from :cl-arrows :->>))
(in-package :vase.db.sqlite3.folder)

;; insert
(defclass row ()
  ((folder-id
    :initarg :folder-id
    :reader row-folder-id)
   (name
    :initarg :name
    :reader row-name)
   (modified-at
    :initarg :modified-at
    :reader row-modified-at)))

(defmethod make-row ((db sqlite3-db) folder-id name modified-at)
  (make-instance 'row
                 :folder-id folder-id
                 :name name
                 :modified-at modified-at))

(defmethod insert ((db sqlite3-db) (rows list))
  (->> (mapcar #'list
               (mapcar #'row-folder-id rows)
               (mapcar #'row-name rows)
               (mapcar #'row-modified-at rows))
       (insert-bulk db +folders+
                    (list +folder-id+
                          +folders/name+
                          +folders/modified-at+)))
  db)

(defmethod select ((db sqlite3-db) (ids list))
  (when ids
    (->> (query db
          (join " SELECT"
                "  *"
                " FROM"
                " " +folders+
                " WHERE"
                " " +folder-id+ " in (" (placeholder ids) ")")
          ids)
         (mapcar (lambda (plist)
                   (make-instance 'row
                    :folder-id (getf plist :|folder_id|)
                    :name (getf plist :|name|)
                    :modified-at (getf plist :|modified_at|)))))))

(defmethod select-ids ((db sqlite3-db) offset size)
  (mapcar #'second
          (query db
                 (join " SELECT"
                       " " +folder-id+
                       " FROM"
                       " " +folders+
                       " ORDER BY"
                       " " +folders/modified-at+
                       " DESC"
                       " LIMIT ?,?")
                 (list offset size))))

(defmethod search-ids ((db sqlite3-db) (name string))
  (mapcar #'second
          (query db
                 (join " SELECT"
                       " " +folder-id+
                       " FROM"
                       " " +folders+
                       " WHERE"
                       " " +folders/name+
                       " LIKE ?"
                       " LIMIT 0,10")
                 (list (format nil "%~A%" name)))))

(defmethod delete ((db sqlite3-db) folder-ids)
  (delete-bulk db +folders+ +folder-id+ folder-ids)
  db)
