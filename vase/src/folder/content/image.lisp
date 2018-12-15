(in-package :vase.folder.content)

;;; Image content
(defclass image (content vase.image:image) ())

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
