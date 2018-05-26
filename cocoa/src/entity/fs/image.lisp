;;; A file representation based on the local file system
(defpackage :cocoa.entity.fs.image
  (:use :cl))
(in-package :cocoa.entity.fs.image)
(cl-annot:enable-annot-syntax)

(defstruct image-row image-id path)
(export 'make-image-row)
(export 'image-row-image-id)
(export 'image-row-path)

@export
(defgeneric image-insert (dao rows))
@export
(defgeneric image-select (dao ids))
@export
(defgeneric image-delete (dao ids))


(defclass image ()
  ((row :initarg :row :reader image-row)))

@export
(defun image-id (image)
  (image-row-image-id (image-row image)))

@export
(defun image-path (image)
  (image-row-path (image-row image)))


(defun row->image (row)
  (make-instance 'image :row row))

@export
(defun make-images/paths (digest-fn paths)
  (let ((ids (mapcar digest-fn paths)))
    (loop for id in ids
          for path in paths
          collect (row->image
                   (make-image-row :image-id id :path path)))))

@export
(defun save-images (dao images)
  (image-insert dao (mapcar #'image-row images))
  (values))

@export
(defun list-images/ids (dao ids)
  (mapcar #'row->image (image-select dao ids)))

@export
(defun delete-images/ids (dao ids)
  (image-delete dao ids)
  (values))
