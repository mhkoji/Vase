(defpackage :vase.entities.folder.content.db
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :vase.entities.folder.content.db)
(cl-annot:enable-annot-syntax)

@export
(defgeneric folder-content-insert (db folder-id content-id-list))
@export
(defgeneric folder-content-select-ids (db folder-id))
@export
(defgeneric folder-content-delete (db folder-id-list))
