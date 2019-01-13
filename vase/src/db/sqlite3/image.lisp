(defpackage :vase.db.sqlite3.image
  (:use :cl :vase.db.image
        :vase.db.sqlite3)
  (:shadowing-import-from :vase.db.image :delete)
  (:import-from :cl-arrows :->>))
(in-package :vase.db.sqlite3.image)

(defmethod insert ((db sqlite3-db) (rows list))
  (->> (mapcar #'list
               (mapcar #'row-image-id rows)
               (mapcar #'row-path rows))
       (insert-bulk db +images+ (list +image-id+ +images/path+))))

(defmethod select ((db sqlite3-db) (ids list))
  (when ids
    (mapcar (lambda (plist)
              (make-row
               :image-id (getf plist :|image_id|)
               :path (getf plist :|path|)))
     (query db
      (join " SELECT"
            " " +image-id+ ", " +images/path+
            " FROM"
            " " +images+
            " WHERE"
            " " +image-id+ " in (" (placeholder ids) ")")
      ids))))

(defmethod delete ((db sqlite3-db) (ids list))
  (delete-bulk db +images+ +image-id+ ids)
  db)
