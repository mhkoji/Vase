(defpackage :cocoa.db.sqlite3.folder
  (:use :cl
        :cocoa.entity.folder
        :cocoa.db.sqlite3)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.db.sqlite3.folder)

;; insert
(defstruct folder-row %folder-id %name %modified-at)

(defmethod folder-row ((dao sqlite3-dao) folder-id name modified-at)
  (make-folder-row :%folder-id folder-id
                   :%name name
                   :%modified-at modified-at))

(defmethod folder-row-folder-id ((row folder-row))
  (folder-row-%folder-id row))

(defmethod folder-row-name ((row folder-row))
  (folder-row-%name row))

(defmethod folder-row-modified-at ((row folder-row))
  (folder-row-%modified-at row))


(defmethod folder-insert ((dao sqlite3-dao) (rows list))
  (->> (mapcar #'list
               (mapcar #'folder-row-folder-id rows)
               (mapcar #'folder-row-name rows)
               (mapcar #'folder-row-modified-at rows))
       (insert-bulk dao +folders+
                    (list +folder-id+
                          +folders/name+
                          +folders/modified-at+)))
  dao)

(defmethod folder-select ((dao sqlite3-dao) (ids list))
  (when ids
    (->> (query dao
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

  (defmethod folder-select-ids ((dao sqlite3-dao) offset size)
  (mapcar #'second
          (query dao
                 (join " SELECT"
                       " " +folder-id+
                       " FROM"
                       " " +folders+
                       " ORDER BY"
                       " " +folders/modified-at+
                       " DESC"
                       " LIMIT ?,?")
                 (list offset size))))

(defmethod folder-search-ids ((dao sqlite3-dao) (name string))
  (mapcar #'second
          (query dao
                 (join " SELECT"
                       " " +folder-id+
                       " FROM"
                       " " +folders+
                       " WHERE"
                       " " +folders/name+
                       " LIKE ?"
                       " LIMIT 0,10")
                 (list (format nil "%~A%" name)))))

(defmethod folder-delete ((dao sqlite3-dao) folder-ids)
  (delete-bulk dao +folders+ +folder-id+ folder-ids)
  dao)


(defstruct thumbnail-row %folder-id %thumbnail-id)

(defmethod thumbnail-row ((dao sqlite3-dao) folder-id thumbnail-id)
  (make-thumbnail-row :%folder-id folder-id :%thumbnail-id thumbnail-id))

(defmethod thumbnail-row-folder-id ((row thumbnail-row))
  (thumbnail-row-%folder-id row))

(defmethod thumbnail-row-thumbnail-id ((row thumbnail-row))
  (thumbnail-row-%thumbnail-id row))

(defmethod folder-thumbnail-insert ((dao sqlite3-dao) (rows list))
  (->> (mapcar #'list
               (mapcar #'thumbnail-row-folder-id rows)
               (mapcar #'thumbnail-row-thumbnail-id rows))
       (insert-bulk dao +folder-thumbnails+
                    (list +folder-id+
                          +thumbnail-id+)))
  dao)

(defmethod folder-thumbnail-select ((dao sqlite3-dao) (ids list))
  (when ids
    (->> (query dao
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

(defmethod folder-thumbnail-delete ((dao sqlite3-dao) folder-ids)
  (delete-bulk dao +folder-thumbnails+ +folder-id+ folder-ids)
  dao)
