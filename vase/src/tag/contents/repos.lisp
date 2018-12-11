(defpackage :vase.tag.contents.repos
  (:use :cl)
  (:export :bulk-load))
(in-package :vase.tag.contents.repos)

(defgeneric bulk-load (repos type content-ids))
