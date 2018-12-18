(defpackage :vase.entities.folder.repository
  (:use :cl)
  (:export :load-all :save-all))
(in-package :vase.entities.folder.repository)

(defun load-all (db)
  (funcall db :folder.load-all))

(defun save-all (db folders)
  (funcall db :folder.save-all folders))
