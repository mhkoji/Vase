(defpackage :cocoa.use-case.folder.thumbnail
  (:use :cl)
  (:import-from :cocoa.folder
                :thumbnail
                :thumbnail-id))
(in-package :cocoa.use-case.folder.thumbnail)
(cl-annot:enable-annot-syntax)

;;; A thumbnail implementation by some image id
;;; Implemented as a plug-in to the folder
(defclass image-thumbnail (thumbnail) ())

@export
(defun make-of-image (image-id)
  "Make the image instance from the given image-id"
  (make-instance 'image-thumbnail :id image-id))

@export
(defun thumbnail->image-id (thumbnail)
  "Extract the image id of the thumbnail, if any"
  (thumbnail-id thumbnail))
