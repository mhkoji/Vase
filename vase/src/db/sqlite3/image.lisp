(defpackage :vase.db.sqlite3.fs.image
  (:use :cl
        :vase.db.sqlite3
        :vase.entities.fs.image.db)
  (:import-from :cl-arrows :->>)
  (:import-from :vase.entities.fs.image
                :make-image
                :image
                :image-id
                :image-path))
(in-package :vase.db.sqlite3.fs.image)

(defmethod image-insert ((db sqlite3-db) (images list))
  (->> (mapcar #'list
               (mapcar #'image-id images)
               (mapcar #'image-path images))
       (insert-bulk db +images+ (list +image-id+ +images/path+))))

(defmethod image-row-image-id ((plist list))
  (getf plist :|image_id|))

(defmethod image-row-path ((plist list))
  (getf plist :|path|))

(defmethod image-select ((db sqlite3-db) (ids list))
  (when ids
    (query db
      (join " SELECT"
            " " +image-id+ ", " +images/path+
            " FROM"
            " " +images+
            " WHERE"
            " " +image-id+ " in (" (placeholder ids) ")")
      ids)))

(defmethod image-delete ((db sqlite3-db) (ids list))
  (delete-bulk db +images+ +image-id+ ids)
  db)
