(defpackage :cocoa.use-case.tag.contents.folder
  (:use :cl
        :cocoa.use-case.tag.contents
        :cocoa.entity.tag))
(in-package :cocoa.use-case.tag.contents.folder)
(cl-annot:enable-annot-syntax)

(defun as-content (folder-id)
  (make-simple-content folder-id :folder))

(defmethod list-typed-contents ((container list)
                                (type (eql :folder))
                                (content-ids list))
  (cocoa.use-case.folder.list:list/ids
   content-ids
   :folder-repository (getf container :folder)))

@export
(defun set-tags! (folder-id tag-ids &key tag-repository)
  (let ((content (as-content folder-id)))
    (dolist (tag (list-tags/content tag-repository content))
      (detach-tag tag content))
    (dolist (tag (list-tags/ids tag-repository tag-ids))
      (attach-tag tag content))))

@export
(defun tags (folder-id &key tag-repository)
  (cocoa.use-case.tag:list/content
   (as-content folder-id)
   :tag-repository tag-repository))
