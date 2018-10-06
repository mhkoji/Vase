(defpackage :vase.entities.fs.image.db
  (:use :cl :vase.entities.fs.image))
(in-package :vase.entities.fs.image.db)
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

