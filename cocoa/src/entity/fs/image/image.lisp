;;; A file representation based on the local file system
(defpackage :cocoa.entity.fs.image
  (:use :cl)
  (:import-from :cl-arrows :-> :->>))
(in-package :cocoa.entity.fs.image)

(defclass image ()
  ((id :initarg :id)
   (path :initarg :path)))

(defun image-id (image)
  (slot-value image 'id))
(export 'image-id)

(defun image-path (image)
  (slot-value image 'path))
(export 'image-path)

(defun make-image (id path)
  (make-instance 'image :id id :path path))
(export 'make-image)
