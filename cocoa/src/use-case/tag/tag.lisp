(defpackage :cocoa.use-case.tag
  (:use :cl :cocoa.entity.tag))
(in-package :cocoa.use-case.tag)
(cl-annot:enable-annot-syntax)

(defun tag->dto (tag)
  (list :id (tag-id tag) :name (tag-name tag)))

@export
(defun create/name (name &key tag-dao)
  (save-tag tag-dao (make-tag/name tag-dao name))
  (values))

@export
(defun list/range (from size &key tag-dao)
  (mapcar #'tag->dto (list-tags/range tag-dao from size)))

@export
(defun list/content (content &key tag-dao)
  (mapcar #'tag->dto (list-tags/content tag-dao content)))

@export
(defun delete/id (tag-id &key tag-dao)
  (delete-tags/ids tag-dao (list tag-id))
  (values))

@export
(defun change-name (tag-id name &key tag-dao)
  (let ((tag (car (list-tags/ids tag-dao (list tag-id)))))
    (when tag
      (setf (tag-name tag) name)
      (save-tag tag-dao tag)))
  (values))
