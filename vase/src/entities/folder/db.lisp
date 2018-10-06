(defpackage :vase.entities.folder.db
  (:use :cl))
(in-package :vase.entities.folder.db)
(cl-annot:enable-annot-syntax)

;;; A primitive language to access the db
@export
(defgeneric folder-select (db ids))
@export
(defgeneric make-folder-row (db folder-id name modified-at))
@export
(defgeneric folder-row-folder-id (folder-row))
@export
(defgeneric folder-row-name (folder-row))
@export
(defgeneric folder-row-modified-at (folder-row))

@export
(defgeneric folder-select-ids (db offset size))
@export
(defgeneric folder-search-ids (db keyword))
@export
(defgeneric folder-insert (db folder-rows))
@export
(defgeneric folder-delete (db folder-id-list))

@export
(defgeneric folder-thumbnail-select (db folder-id-list))
@export
(defgeneric thumbnail-row-folder-id (thumbnail-row))
@export
(defgeneric thumbnail-row-thumbnail-id (thumbnail-row))
@export
(defgeneric make-thumbnail-row (db folder-id thumbnail-id))
@export
(defgeneric folder-thumbnail-insert (db thumbnail-row-list))
@export
(defgeneric folder-thumbnail-delete (db folder-id-list))
