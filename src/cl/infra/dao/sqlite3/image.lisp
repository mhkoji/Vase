(defpackage :cocoa.infra.dao.sqlite3.image
  (:use :cl
        :cocoa.infra.dao.sqlite3
        :cocoa.entity.image.dao))
(in-package :cocoa.infra.dao.sqlite3.image)

(defmethod image-insert ((dao sqlite3-dao) (rows list))
  (insert-bulk dao "images" '("image_id" "path")
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
              (join "SELECT"
                    " image_id, path"
                    "FROM"
                    " images"
                    "WHERE"
                    " image_id in (" (placeholder ids) ")")
              ids)))))

(defmethod image-delete ((dao sqlite3-dao) (ids list))
  (delete-bulk dao "images" "image_id" ids))
