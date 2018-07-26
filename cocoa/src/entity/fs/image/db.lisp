(defpackage :cocoa.entity.fs.image.db
  (:use :cl :cocoa.entity.fs.image))
(in-package :cocoa.entity.fs.image.db)
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

