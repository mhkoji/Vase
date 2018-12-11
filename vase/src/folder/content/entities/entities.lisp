(defpackage :vase.folder.content.entities
  (:use :cl)
  (:export :image
           :from-image)
  (:import-from :vase.folder.content.entities.repos
                :bulk-load))
(in-package :vase.folder.content.entities)

(defclass image (vase.folder:content
                 vase.image:image) ())

(defun from-image (image)
  (assert (typep image 'vase.image:image))
  (change-class image 'image
                :type :image
                :get-entity-id #'vase.image:image-id))

(defmethod bulk-load ((repos vase.image.repos:repository)
                      (type (eql :image))
                      (entity-ids list))
  (mapcar #'from-image
          (vase.image.repos:bulk-load-by-ids repos entity-ids)))
