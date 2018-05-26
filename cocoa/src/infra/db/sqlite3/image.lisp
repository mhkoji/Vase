(defpackage :cocoa.infra.db.sqlite3.fs.image
  (:use :cl
        :cocoa.entity.fs.image
        :cocoa.infra.db.sqlite3))
(in-package :cocoa.infra.db.sqlite3.fs.image)

(defmethod image-insert ((dao sqlite3-dao) (rows list))
  (insert-bulk dao +images+ (list +image-id+ +images/path+)
   (mapcar #'list
           (mapcar #'image-row-image-id rows)
           (mapcar #'image-row-path rows))))

(labels ((plist->image-row (plist)
           (make-image-row :path (getf plist :|path|)
                           :image-id (getf plist :|image_id|))))
(defmethod image-select ((dao sqlite3-dao) (ids list))
  (when ids
    (mapcar
     #'plist->image-row
     (query dao
            (join " SELECT"
                  " " +image-id+ ", " +images/path+
                  " FROM"
                  " " +images+
                  " WHERE"
                  " " +image-id+ " in (" (placeholder ids) ")")
            ids)))))

(defmethod image-delete ((dao sqlite3-dao) (ids list))
  (delete-bulk dao +images+ +image-id+ ids))
