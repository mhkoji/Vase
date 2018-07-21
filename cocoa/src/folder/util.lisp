(defpackage :cocoa.folder.util
  (:use :cl))
(in-package :cocoa.folder.util)
(cl-annot:enable-annot-syntax)

@export
(defun accept-folder-id (folder-id)
  (assert (typep folder-id 'string))
  folder-id)
