(defpackage :cocoa.infra.dao.sqlite3.folder
  (:use :cl
        :cocoa.infra.dao.sqlite3
        :cocoa.entity.folder.dao))
(in-package :cocoa.infra.dao.sqlite3.folder)

;; insert
(defmethod folder-insert ((dao sqlite3-dao) (rows list))
  (insert-bulk dao "folders" '("folder_id" "name" "modified_at")
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
                   (join "SELECT"
                         "  *"
                         "FROM"
                         "  folders"
                         "WHERE"
                         "  folder_id in (" (placeholder ids) ")")
                   ids))))

(defmethod folder-select-ids ((dao sqlite3-dao) offset size)
  (mapcar #'second
          (query dao
                 (join "SELECT"
                       "  folder_id"
                       "FROM"
                       "  folders"
                       "ORDER BY modified_at DESC"
                       "LIMIT ?,?")
                 (list offset size))))

(defmethod folder-delete ((dao sqlite3-dao) folder-ids)
  (delete-bulk dao "folders" "folder_id" folder-ids)
  dao)


(defmethod folder-content-insert ((dao sqlite3-dao)
                                  (folder-id string)
                                  (content-id-list list))
  (insert-bulk dao "folder_contents" '("folder_id" "content_id")
   (mapcar (lambda (content-id) (list folder-id content-id))
           content-id-list))
  dao)

(defmethod folder-content-select-ids ((dao sqlite3-dao) folder-id)
  (let ((q "SELECT content_id FROM folder_contents WHERE folder_id = ?"))
    (mapcar #'second (query dao q (list folder-id)))))

(defmethod folder-content-delete ((dao sqlite3-dao) folder-ids)
  (delete-bulk dao "folder_contents" "folder_id" folder-ids)
  dao)


(defmethod folder-thumbnail-insert ((dao sqlite3-dao) (rows list))
  (insert-bulk dao "folder_thumbnails" '("folder_id" "thumbnail_id")
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
            (join "SELECT"
                  "  folder_id, thumbnail_id"
                  "FROM"
                  "  folder_thumbnails"
                  "WHERE"
                  "  folder_id in (" (placeholder ids) ")")
            ids))))

(defmethod folder-thumbnail-delete ((dao sqlite3-dao) folder-ids)
  (delete-bulk dao "folder_thumbnails" "folder_id" folder-ids)
  dao)
