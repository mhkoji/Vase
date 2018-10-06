(defpackage :vase.db.sqlite3.folder
  (:use :cl
        :vase.db.sqlite3
        :vase.entities.folder.db
        :vase.entities.folder.content.db)
  (:import-from :cl-arrows :->>))
(in-package :vase.db.sqlite3.folder)

;; insert
(defclass folder-row ()
  ((folder-id
    :initarg :folder-id
    :reader folder-row-folder-id)
   (name
    :initarg :name
    :reader folder-row-name)
   (modified-at
    :initarg :modified-at
    :reader folder-row-modified-at)))

(defmethod make-folder-row ((db sqlite3-db) folder-id name modified-at)
  (make-instance 'folder-row
                 :folder-id folder-id
                 :name name
                 :modified-at modified-at))

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
                   (make-instance 'folder-row
                    :folder-id (getf plist :|folder_id|)
                    :name (getf plist :|name|)
                    :modified-at (getf plist :|modified_at|)))))))

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


(defclass thumbnail-row ()
  ((folder-id
    :initarg :folder-id
    :reader thumbnail-row-folder-id)
   (thumbnail-id
    :initarg :thumbnail-id
    :reader thumbnail-row-thumbnail-id)))

(defmethod make-thumbnail-row ((db sqlite3-db) folder-id thumbnail-id)
  (make-instance 'thumbnail-row
                 :folder-id folder-id :thumbnail-id thumbnail-id))

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
                   (make-instance 'thumbnail-row
                    :folder-id (getf plist :|folder_id|)
                    :thumbnail-id (getf plist :|thumbnail_id|)))))))

(defmethod folder-thumbnail-delete ((db sqlite3-db) folder-ids)
  (delete-bulk db +folder-thumbnails+ +folder-id+ folder-ids)
  db)
