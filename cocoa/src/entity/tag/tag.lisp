;;; Tag
(in-package :cocoa.entity.tag)
(cl-annot:enable-annot-syntax)

@export
(defgeneric tag-id (tag))
@export
(defgeneric tag-name (tag))
@export
(defgeneric (setf tag-name) (name tag))
@export
(defgeneric tag-contents (tag))

@export
(defgeneric attach-tag (tag content))
@export
(defgeneric detach-tag (tag content))

@export
(defgeneric make-tag/name (factory name))

@export
(defgeneric save-tag (repository tag))
@export
(defgeneric list-tags/range (repository offset size))
@export
(defgeneric list-tags/ids (repository ids))
@export
(defgeneric list-tags/content (repository content))
@export
(defgeneric delete-tags/ids (repository ids))
