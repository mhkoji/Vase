;;; A file representation based on the local file system
(defpackage :cocoa.entity.fs.image
  (:use :cl)
  (:import-from :cl-arrows :-> :->>))
(in-package :cocoa.entity.fs.image)
(cl-annot:enable-annot-syntax)

(defclass image ()
  ((id :initarg :id)
   (path :initarg :path)))

@export
(defun image-id (image)
  (slot-value image 'id))

@export
(defun image-path (image)
  (slot-value image 'path))

@export
(defun make-image (id path)
  (make-instance 'image :id id :path path))
