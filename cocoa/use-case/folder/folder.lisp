(defpackage :cocoa.use-case.folder
  (:use :cl)
  (:import-from :cl-arrows :-> :->> :-<>))
(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

@export
(defun content->image-dto (content)
  (list :id (cocoa.use-case.folder.content:content->image-id content)))

(defun thumbnail->dto (thumbnail)
  (list :id (cocoa.use-case.folder.thumbnail:thumbnail->image-id
             thumbnail)))

@export
(defun response (folder)
  (list :id (cocoa.folder:folder-id folder)
        :name (cocoa.folder:folder-name folder)
        :thumbnail (thumbnail->dto (cocoa.folder:folder-thumbnail folder))))
