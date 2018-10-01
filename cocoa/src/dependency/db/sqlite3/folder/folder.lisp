(defpackage :cocoa.dependency.db.sqlite3.folder
  (:use :cl
        :cocoa.dependency.db.sqlite3
        :cocoa.entity.folder.db
        :cocoa.entity.folder.content.db)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.dependency.db.sqlite3.folder)

;; insert
(defstruct folder-row %folder-id %name %modified-at)

(defmethod folder-row ((db sqlite3-db) folder-id name modified-at)
  (make-folder-row :%folder-id folder-id
                   :%name name
                   :%modified-at modified-at))

(defmethod folder-row-folder-id ((row folder-row))
  (folder-row-%folder-id row))

(defmethod folder-row-name ((row folder-row))
  (folder-row-%name row))

(defmethod folder-row-modified-at ((row folder-row))
  (folder-row-%modified-at row))


(defmethod folder-insert ((db sqlite3-db) (rows list))
  (->> (mapcar #'list
               (mapcar #'folder-row-folder-id rows)
               (mapcar #'folder-row-name rows)
               (mapcar #'folder-row-modified-at rows))
       (insert-bulk db +folders+
                    (list +folder-id+
                          +folders/name+
                          +folders/modified-at+)))
  db)

(defmethod folder-select ((db sqlite3-db) (ids list))
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
                   (make-folder-row
                    :%folder-id (getf plist :|folder_id|)
                    :%name (getf plist :|name|)
                    :%modified-at (getf plist :|modified_at|)))))))

(defmethod folder-select-ids ((db sqlite3-db) offset size)
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

(defmethod folder-search-ids ((db sqlite3-db) (name string))
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

(defmethod folder-delete ((db sqlite3-db) folder-ids)
  (delete-bulk db +folders+ +folder-id+ folder-ids)
  db)


(defstruct thumbnail-row %folder-id %thumbnail-id)

(defmethod thumbnail-row ((db sqlite3-db) folder-id thumbnail-id)
  (make-thumbnail-row :%folder-id folder-id :%thumbnail-id thumbnail-id))

(defmethod thumbnail-row-folder-id ((row thumbnail-row))
  (thumbnail-row-%folder-id row))

(defmethod thumbnail-row-thumbnail-id ((row thumbnail-row))
  (thumbnail-row-%thumbnail-id row))

(defmethod folder-thumbnail-insert ((db sqlite3-db) (rows list))
  (->> (mapcar #'list
               (mapcar #'thumbnail-row-folder-id rows)
               (mapcar #'thumbnail-row-thumbnail-id rows))
       (insert-bulk db +folder-thumbnails+
                    (list +folder-id+
                          +thumbnail-id+)))
  db)

(defmethod folder-thumbnail-select ((db sqlite3-db) (ids list))
  (when ids
    (->> (query db
          (join " SELECT"
                " " +folder-id+ ", " +thumbnail-id+
                " FROM"
                " "  +folder-thumbnails+
                " WHERE"
                " " +folder-id+ " in (" (placeholder ids) ")")
          ids)
         (mapcar (lambda (plist)
                   (make-thumbnail-row
                    :%folder-id (getf plist :|folder_id|)
                    :%thumbnail-id (getf plist :|thumbnail_id|)))))))

(defmethod folder-thumbnail-delete ((db sqlite3-db) folder-ids)
  (delete-bulk db +folder-thumbnails+ +folder-id+ folder-ids)
  db)
