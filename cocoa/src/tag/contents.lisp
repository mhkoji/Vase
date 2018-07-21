(defpackage :cocoa.tag.contents
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.tag.contents)
(cl-annot:enable-annot-syntax)

@export
(defun get-folders (tag-id &key tag-repository folder-repository)
  (cocoa.entity.tag:load-rendered-contents-by-tag
   (car (cocoa.entity.tag:load-tags-by-ids
         tag-repository
         (list tag-id)))
   (make-instance 'cocoa.folder.tag:folder-container
    :folder-repository folder-repository)))
