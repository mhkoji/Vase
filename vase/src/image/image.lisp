(defpackage :vase.image
  (:use :cl)
  (:export :image
           :image-id
           :image-path
           :bulk-create)
  (:import-from :vase.image.repos
                :image
                :image-id
                :image-path))
(in-package :vase.image)

(defun bulk-create (id-generator paths)
  (let ((ids (mapcar (lambda (p)
                       (vase.id:gen id-generator p))
                     paths)))
    (mapcar (lambda (id p)
              (make-instance 'image :id id :path p))
            ids paths)))
