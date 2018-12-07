(defpackage :vase.image
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
(in-package :vase.image)

(defclass image ()
  ((id :initarg :id
       :reader image-id)
   (path :initarg :path
         :reader image-path)))


(defstruct factory id-generator)

(defun bulk-create (factory paths)
  (let ((id-generator (factory-id-generator factory)))
    (let ((ids (mapcar (lambda (p)
                         (vase.id:gen id-generator p))
                       paths)))
      (mapcar (lambda (id p)
                (make-instance 'image :id id :path p))
              ids paths))))


(defun image->image-row (image)
  (vase.db.image:make-row
   :image-id (vase.image:image-id image)
   :path (vase.image:image-path image)))


(defstruct repository db)

(defun bulk-save (repos images)
  (vase.db.image:insert (repository-db repos)
                        (mapcar #'image->image-row images)))

(defun image-row->image (image-row)
  (make-instance 'vase.image:image
                 :id (vase.db.image:row-image-id image-row)
                 :path (vase.db.image:row-path image-row)))

(defun bulk-load-by-ids (repos ids)
  (mapcar #'image-row->image (vase.db.image:select
                              (repository-db repos)
                              ids)))

(defun bulk-delete (repos image-ids)
  (vase.db.image:delete (repository-db repos) image-ids))
