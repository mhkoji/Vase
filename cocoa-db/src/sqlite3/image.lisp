(defpackage :cocoa.db.sqlite3.fs.image
  (:use :cl
        :cocoa.fs.image
        :cocoa.db.sqlite3)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.db.sqlite3.fs.image)

(defmethod image-insert ((dao sqlite3-dao) (images list))
  (->> (mapcar #'list
               (mapcar #'image-id images)
               (mapcar #'image-path images))
       (insert-bulk dao +images+ (list +image-id+ +images/path+))))

(defmethod image-row-image-id ((plist list))
  (getf plist :|image_id|))

(defmethod image-row-path ((plist list))
  (getf plist :|path|))

(defmethod image-select ((dao sqlite3-dao) (ids list))
  (when ids
    (query dao
      (join " SELECT"
            " " +image-id+ ", " +images/path+
            " FROM"
            " " +images+
            " WHERE"
            " " +image-id+ " in (" (placeholder ids) ")")
      ids)))

(defmethod image-delete ((dao sqlite3-dao) (ids list))
  (delete-bulk dao +images+ +image-id+ ids)
  dao)
