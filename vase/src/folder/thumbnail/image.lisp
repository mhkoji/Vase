(in-package :vase.folder.thumbnail)

(defmethod thumbnail-id ((th vase.image:image))
  (vase.image:image-id th))

(defmethod bulk-load ((repos vase.image:repository)
                      (thumbnail-ids list))
  (vase.image.repos:bulk-load-by-ids repos thumbnail-ids))

(defmethod bulk-delete ((repos vase.image.repos:repository)
                        (thumbnail-ids list))
  (vase.image.repos:bulk-delete repos thumbnail-ids))
