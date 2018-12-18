(defpackage :vase.entities.tag-edit.repository
  (:use :cl)
  (:shadow :load)
  (:export :load :save))
(in-package :vase.entities.tag-edit.repository)

(defun load (db)
  (funcall db :tag-edit.load))

(defun save (db edit)
  (funcall db :tag-edit.save edit))
