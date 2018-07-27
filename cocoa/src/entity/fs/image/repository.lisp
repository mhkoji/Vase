(defpackage :cocoa.entity.fs.image.repository
  (:use :cl
        :cocoa.entity.fs.image
        :cocoa.entity.fs.image.db))
(in-package :cocoa.entity.fs.image.repository)

(defun save-bulk (db images)
  (when images
    (image-insert db images)))
(export 'save-bulk)

#+nil
(defun delete-bulk (db ids)
  (image-delete db ids))
(export 'delete-bulk)

(defun image-row-image (image-row)
  (make-image (image-row-image-id image-row)
              (image-row-path image-row)))

(defun load-by-ids (db ids)
  (mapcar #'image-row-image (image-select db ids)))
(export 'load-by-ids)

