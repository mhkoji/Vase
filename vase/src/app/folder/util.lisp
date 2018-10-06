(defpackage :vase.app.folder.util
  (:use :cl))
(in-package :vase.app.folder.util)
(cl-annot:enable-annot-syntax)

@export
(defun accept-folder-id (folder-id)
  (assert (typep folder-id 'string))
  folder-id)
