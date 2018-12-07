(defpackage :vase.folder.thumbnail
  (:use :cl)
  (:export :image
           :from-image
           :make-db-repos)
  (:import-from :vase.folder.thumbnail.repos
                :bulk-load
                :bulk-delete))
(in-package :vase.folder.thumbnail)

(defclass image (vase.folder:thumbnail vase.image:image) ())

(defun from-image (image)
  (assert (typep image 'vase.image:image))
  (change-class image 'image
                :get-id #'vase.image:image-id))


(defmethod bulk-load ((repos vase.image:repository) (thumbnail-ids list))
  (mapcar #'from-image
          (vase.image:bulk-load-by-ids repos thumbnail-ids)))

(defmethod bulk-delete ((repos vase.image:repository) (thumbnail-ids list))
  (vase.image:bulk-delete repos thumbnail-ids))
