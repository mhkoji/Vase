(defpackage :cocoa.use-case.tag.contents.folder
  (:use :cl
        :cocoa.use-case.tag.contents
        :cocoa.entity.tag))
(in-package :cocoa.use-case.tag.contents.folder)
(cl-annot:enable-annot-syntax)

(defclass simple-content ()
  ((id
    :initarg :id
    :reader content-id)
   (type
    :initarg :type
    :type :keyword
    :reader content-type)))

@export
(defun make-simple-content (id type)
  (make-instance 'simple-content :id id :type type))

(defun as-content (folder-id)
  (make-simple-content folder-id :folder))

(defmethod list-typed-contents ((container list)
                                (type (eql :folder))
                                (content-ids list))
  (cocoa.use-case.folder:list-by-ids content-ids
   :folder-dao (getf container :folder)))

@export
(defun set-tags! (folder-id tag-ids &key tag-dao)
  (let ((content (as-content folder-id)))
    (dolist (tag (list-tags/content tag-dao content))
      (detach-tag tag content))
    (dolist (tag (list-tags/ids tag-dao tag-ids))
      (attach-tag tag content))))

@export
(defun tags (folder-id &key tag-dao)
  (cocoa.use-case.tag:list/content (as-content folder-id) :tag-dao tag-dao))
