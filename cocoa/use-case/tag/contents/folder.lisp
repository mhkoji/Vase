(defpackage :cocoa.use-case.tag.contents.folder
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.use-case.tag.contents.folder)
(cl-annot:enable-annot-syntax)

@export
(defclass container ()
  ((folder-repository :initarg :folder-repository)))

(defmethod cocoa.tag:render-contents ((container container)
                                      (type (eql :folder))
                                      (content-ids list))
  (cocoa.use-case.folder:list-by-ids content-ids
   :folder-repository (slot-value container 'folder-repository)))

@export
(defun get-folders (tag-id &key tag-repository folder-repository)
  (cocoa.tag:load-rendered-contents-by-tag
   (car (cocoa.tag:load-tags-by-ids
         tag-repository
         (list tag-id)))
   (make-instance 'container :folder-repository folder-repository)))
