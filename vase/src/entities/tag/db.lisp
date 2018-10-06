(defpackage :vase.entities.tag.db
  (:use :cl :vase.entities.tag))
(in-package :vase.entities.tag.db)
(cl-annot:enable-annot-syntax)

(defstruct tag-row tag-id name)
(export 'make-tag-row)
(export 'tag-row)
(export 'tag-row-tag-id)
(export 'tag-row-name)

(defstruct content-row id type)
(export 'make-content-row)
(export 'content-row)
(export 'content-row-id)
(export 'content-row-type)

@export
(defgeneric tag-insert (db name))
@export
(defgeneric tag-delete (db tag-id-list))
@export
(defgeneric tag-update (db tag-row))
@export
(defgeneric tag-select/ids (db ids))
@export
(defgeneric tag-select/range (db offset size))

@export
(defgeneric tag-content-insert (db content-row tag-id-list))
@export
(defgeneric tag-content-delete (db content-row tag-id-list))
@export
(defgeneric tag-content-select-tags (db content-row))
@export
(defgeneric tag-content-select-contents (db tag-id))
