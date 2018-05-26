(defpackage :cocoa.infra.db.folder.sqlite3
  (:use :cl
        :cocoa.infra.db.sqlite3
        :cocoa.infra.db.folder.dao))
(in-package :cocoa.infra.db.folder.sqlite3)

;; insert
(defmethod folder-insert ((dao sqlite3-dao) (rows list))
  (insert-bulk dao +folders+ (list +folder-id+
                                   +folders/name+
                                   +folders/modified-at+)
   (mapcar #'list
           (mapcar #'folder-row-id rows)
           (mapcar #'folder-row-name rows)
           (mapcar #'folder-row-modified-at rows)))
  dao)

(defmethod folder-select ((dao sqlite3-dao) (ids list))
  (when ids
    (mapcar (lambda (plist)
              (make-folder-row :id (getf plist :|folder_id|)
                               :name (getf plist :|name|)
                               :modified-at (getf plist :|modified_at|)))
            (query dao
                   (join " SELECT"
                         "  *"
                         " FROM"
                         " " +folders+
                         " WHERE"
                         " " +folder-id+ " in (" (placeholder ids) ")")
                   ids))))

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


(defmethod folder-thumbnail-insert ((dao sqlite3-dao) (rows list))
  (insert-bulk dao +folder-thumbnails+ (list +folder-id+ +thumbnail-id+)
   (mapcar #'list
           (mapcar #'thumbnail-row-folder-id rows)
           (mapcar #'thumbnail-row-thumbnail-id rows)))
  dao)

(defmethod folder-thumbnail-select ((dao sqlite3-dao) (ids list))
  (when ids
    (mapcar (lambda (plist)
              (make-thumbnail-row
               :folder-id (getf plist :|folder_id|)
               :thumbnail-id (getf plist :|thumbnail_id|)))
     (query dao
            (join " SELECT"
                  " " +folder-id+ ", " +thumbnail-id+
                  " FROM"
                  " "  +folder-thumbnails+
                  " WHERE"
                  " " +folder-id+ " in (" (placeholder ids) ")")
            ids))))

(defmethod folder-thumbnail-delete ((dao sqlite3-dao) folder-ids)
  (delete-bulk dao +folder-thumbnails+ +folder-id+ folder-ids)
  dao)
