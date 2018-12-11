(defpackage :vase.folder.content.entities.repos
  (:use :cl)
  (:export :bulk-load))
(in-package :vase.folder.content.entities.repos)

(defgeneric bulk-load (entities-repos content-type entity-ids))
