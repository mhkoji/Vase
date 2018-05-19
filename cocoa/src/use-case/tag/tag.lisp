(defpackage :cocoa.use-case.tag
  (:use :cl :cocoa.entity.tag))
(in-package :cocoa.use-case.tag)
(cl-annot:enable-annot-syntax)

(defun tag->dto (tag)
  (list :id (tag-id tag) :name (tag-name tag)))

@export
(defun create/name (name &key tag-factory tag-repository)
  (save-tag tag-repository (make-tag/name tag-factory name))
  (values))

@export
(defun list/range (from size &key tag-repository)
  (mapcar #'tag->dto (list-tags/range tag-repository from size)))

@export
(defun list/content (content &key tag-repository)
  (mapcar #'tag->dto (list-tags/content tag-repository content)))

@export
(defun delete/id (tag-id &key tag-repository)
  (delete-tags/ids tag-repository (list tag-id))
  (values))

@export
(defun change-name (tag-id name &key tag-repository)
  (let ((tag (car (list-tags/ids tag-repository (list tag-id)))))
    (when tag
      (setf (tag-name tag) name)
      (save-tag tag-repository tag)))
  (values))
