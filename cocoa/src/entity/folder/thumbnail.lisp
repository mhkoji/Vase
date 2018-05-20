(defpackage :cocoa.entity.folder.thumbnail
  (:use :cl)
  (:import-from :cocoa.entity.folder
                :thumbnail-id))
(in-package :cocoa.entity.folder.thumbnail)
(cl-annot:enable-annot-syntax)

;;; A thumbnail implementation by some image id
;;; Implemented as a plug-in to the folder
(defclass image-thumbnail ()
  ((image-id
    :initarg :image-id
    :reader image-thumbnail-image-id)))

(defmethod thumbnail-id ((thumbnail image-thumbnail))
  (image-thumbnail-image-id thumbnail))


@export
(defun make-image-thumbnail (image-id)
  "Make the image instance from the given image-id"
  (make-instance 'image-thumbnail :image-id image-id))

@export
(defun thumbnail->image-id (thumbnail)
  "Extract the image id of the thumbnail, if any"
  (thumbnail-id thumbnail))
