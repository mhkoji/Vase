(in-package :vase.folder.content)

;;; Image content
(defmethod content-type ((c vase.image:image))
  :image)

(defmethod content-entity-id ((c vase.image:image))
  (vase.image:image-id c))

(defmethod bulk-load ((repos vase.image.repos:repository)
                      (type (eql :image))
                      (entity-ids list))
  (vase.image.repos:bulk-load-by-ids repos entity-ids))
