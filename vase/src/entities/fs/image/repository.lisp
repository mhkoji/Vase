(defpackage :vase.entities.fs.image.repository
  (:use :cl
        :vase.entities.fs.image
        :vase.entities.fs.image.db))
(in-package :vase.entities.fs.image.repository)

(defun add-bulk (db images)
  (dolist (image images)
    (image-insert db (list image))))
(export 'add-bulk)

(defun delete-bulk (db image-ids)
  (image-delete db image-ids))
(export 'delete-bulk)

(defun image-row-image (image-row)
  (make-image (image-row-image-id image-row)
              (image-row-path image-row)))

(defun load-by-ids (db ids)
  (mapcar #'image-row-image (image-select db ids)))
(export 'load-by-ids)

