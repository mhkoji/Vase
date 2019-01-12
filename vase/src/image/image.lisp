(defpackage :vase.image
  (:use :cl)
  (:export :image
           :image-id
           :image-path
           :bulk-create

           :repository
           :make-repository
           :bulk-save
           :bulk-load-by-ids
           :bulk-delete))
(in-package :vase.image)

(defclass image ()
  ((id :initarg :id
       :reader image-id)
   (path :initarg :path
         :reader image-path)))

(defun bulk-create (id-generator paths)
  (let ((ids (mapcar (lambda (p)
                       (vase.id:gen id-generator p))
                     paths)))
    (mapcar (lambda (id p)
              (make-instance 'image :id id :path p))
            ids paths)))
