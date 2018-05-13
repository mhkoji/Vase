(defpackage :cocoaonate.use-case.folder.delete
  (:use :cl))
(in-package :cocoaonate.use-case.folder.delete)
(cl-annot:enable-annot-syntax)

@export
(defun /id (folder-id &key folder-repository)
  (cocoa.entity.folder:delete-folders/ids
   folder-repository
   (list folder-id)))
