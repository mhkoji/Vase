(defpackage :cocoa.entity.fs.image.repository
  (:use :cl
        :cocoa.entity.fs.image
        :cocoa.entity.fs.image.db))
(in-package :cocoa.entity.fs.image.repository)
(cl-annot:enable-annot-syntax)

@export
(defun save-bulk (db images)
  (when images
    (image-insert db images)))

@export
(defun delete-bulk (db ids)
  (image-delete db ids))

(defun image-row-image (image-row)
  (make-image (image-row-image-id image-row)
              (image-row-path image-row)))

@export
(defun load-by-ids (db ids)
  (mapcar #'image-row-image (image-select db ids)))

