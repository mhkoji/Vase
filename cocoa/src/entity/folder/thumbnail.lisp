;;; The thumbnail of a folder
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

@export
(defgeneric thumbnail-id (thumbnail)
  (:documentation "Returns the unique id of the thumbnail"))
