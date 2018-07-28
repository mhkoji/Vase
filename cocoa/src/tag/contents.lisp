(defpackage :cocoa.tag.contents
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.tag.contents)
(cl-annot:enable-annot-syntax)

(defun get-folders (db tag-id)
  (cocoa.entity.tag:load-rendered-contents-by-tag
   (car (cocoa.entity.tag.repository:load-by-ids db (list tag-id)))
   (make-instance 'cocoa.folder:folder-container :db db)))
(export 'get-folders)
