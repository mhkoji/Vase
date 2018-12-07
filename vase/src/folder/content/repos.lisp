(defpackage :vase.folder.content.repos
  (:use :cl)
  (:export :bulk-load))
(in-package :vase.folder.content.repos)

(defgeneric bulk-load (content-repos content-type entity-ids))
