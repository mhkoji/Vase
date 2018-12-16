(defpackage :vase.folder.thumbnail
  (:use :cl)
  (:export :thumbnail
           :thumbnail-id
           :image
           :from-image

           :bulk-load
           :bulk-delete))
(in-package :vase.folder.thumbnail)

(defclass thumbnail ()
  ((get-id :initarg :get-id
           :reader get-id)))

(defun thumbnail-id (thumbnail)
  (funcall (get-id thumbnail) thumbnail))


(defgeneric bulk-load (repos thumbnail-ids))

(defgeneric bulk-delete (repos thumbnail-ids))
