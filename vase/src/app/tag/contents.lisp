(defpackage :vase.app.tag.contents
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :vase.app.tag.contents)
(cl-annot:enable-annot-syntax)

(defun get-folders (db tag-id)
  (vase.entities.tag:load-rendered-contents-by-tag
   (car (vase.entities.tag.repository:load-by-ids db (list tag-id)))
   (make-instance 'vase.app.folder:folder-container :db db)))
(export 'get-folders)
