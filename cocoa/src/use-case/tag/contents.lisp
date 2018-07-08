(defpackage :cocoa.use-case.tag.contents
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.use-case.tag.contents)
(cl-annot:enable-annot-syntax)

@export
(defun get-folders (tag-id &key tag-repository folder-repository)
  (cocoa.tag:load-rendered-contents-by-tag
   (car (cocoa.tag:load-tags-by-ids
         tag-repository
         (list tag-id)))
   (make-instance 'cocoa.use-case.folder:folder-container
    :folder-repository folder-repository)))
