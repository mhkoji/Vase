(defpackage :cocoa.entity.image.dao
  (:use :cl :cocoa.entity.image))
(in-package :cocoa.entity.image.dao)
(cl-annot:enable-annot-syntax)

(defstruct image-row image-id path)
(export 'make-image-row)
(export 'image-row-image-id)
(export 'image-row-path)

@export
(defclass dao () ())

@export
(defgeneric image-insert (dao rows))
@export
(defgeneric image-select (dao ids))
@export
(defgeneric image-delete (dao ids))

;; A simple implementation of image
(defclass simple-image (image)
  ((id
    :initarg :id
    :reader image-id)
   (path
    :initarg :path
    :reader image-path)))

(defun make-simple-image (id path)
  (make-instance 'simple-image :id id :path path))

(defmethod make-images/paths ((digest-fn function) (paths list))
  (let ((ids (mapcar digest-fn paths)))
    (loop for id in ids
          for path in paths
          collect (make-simple-image id path))))

(defun image->image-row (image)
  (make-image-row :image-id (image-id image)
                  :path (image-path image)))

(defun image-row->simple-image (row)
  (make-simple-image (image-row-image-id row) (image-row-path row)))

(defmethod save-images ((dao dao) (images list))
  (image-insert dao (mapcar #'image->image-row images))
  (values))

(defmethod list-images/ids ((dao dao) (ids list))
  (mapcar #'image-row->simple-image (image-select dao ids)))

(defmethod delete-images/ids ((dao dao) (ids list))
  (image-delete dao ids)
  (values))
