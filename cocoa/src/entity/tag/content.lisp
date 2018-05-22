;;; Content, to which tags are attached
(in-package :cocoa.entity.tag)
(cl-annot:enable-annot-syntax)

@export
(defgeneric content-id (content))

@export
(defgeneric content-type (content))
