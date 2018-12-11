(defpackage :vase.image.repos
  (:use :cl)
  (:export :image
           :image-id
           :image-path
           :make-factory
           :bulk-create
           :repository
           :bulk-save
           :make-repository
           :bulk-load-by-ids
           :bulk-delete))
(in-package :vase.image.repos)

(defclass image ()
  ((id :initarg :id
       :reader image-id)
   (path :initarg :path
         :reader image-path)))

(defstruct repository db)


(defun image->image-row (image)
  (vase.image.repos.db:make-row :image-id (image-id image)
                                :path (image-path image)))

(defun image-row->image (image-row)
  (make-instance 'image
   :id (vase.image.repos.db:row-image-id image-row)
   :path (vase.image.repos.db:row-path image-row)))

(defun bulk-save (repos images)
  (vase.image.repos.db:insert (repository-db repos)
                        (mapcar #'image->image-row images)))

(defun bulk-load-by-ids (repos ids)
  (mapcar #'image-row->image (vase.image.repos.db:select
                              (repository-db repos)
                              ids)))

(defun bulk-delete (repos image-ids)
  (vase.image.repos.db:delete (repository-db repos) image-ids))
