(defpackage :cocoa.entity.fs.image.repository
  (:use :cl :cocoa.entity.fs.image))
(in-package :cocoa.entity.fs.image.repository)
(cl-annot:enable-annot-syntax)

@export
(defgeneric image-insert (db images))
@export
(defgeneric image-delete (db ids))
@export
(defgeneric image-select (db ids))
@export
(defgeneric image-row-image-id (image-row))
@export
(defgeneric image-row-path (image-path))

@export
(defun save-bulk (db images)
  (image-insert db images))

@export
(defun delete-bulk (db ids)
  (image-delete db ids))

(defun image-row-image (image-row)
  (make-image (image-row-image-id image-row)
              (image-row-path image-row)))

@export
(defun load-by-ids (db ids)
  (mapcar #'image-row-image (image-select db ids)))

