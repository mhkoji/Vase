(defpackage :vase.entities.gui.folder
  (:use :cl)
  (:export :show))
(in-package :vase.entities.gui.folder)

(defun show (gui db)
  (funcall gui :folder.show db))
