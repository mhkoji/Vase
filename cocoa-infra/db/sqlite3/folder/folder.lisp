(defpackage :cocoa.infra.db.sqlite3.folder
  (:use :cl
        :cocoa.folder
        :cocoa.infra.db.sqlite3)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.infra.db.sqlite3.folder)

;; insert
(defmethod folder-insert ((dao sqlite3-dao) (configs list))
  (->> (mapcar #'list
               (mapcar #'folder-config-id configs)
               (mapcar #'folder-config-name configs)
               (mapcar #'folder-config-modified-at configs))
       (insert-bulk dao +folders+
                    (list +folder-id+
                          +folders/name+
                          +folders/modified-at+)))
  dao)

(defmethod folder-row-folder-id ((plist list))
  (getf plist :|folder_id|))

(defmethod folder-row-name ((plist list))
  (getf plist :|name|))

(defmethod folder-select ((dao sqlite3-dao) (ids list))
  (when ids
    (query dao
           (join " SELECT"
                 "  *"
                 " FROM"
                 " " +folders+
                 " WHERE"
                 " " +folder-id+ " in (" (placeholder ids) ")")
           ids)))

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
