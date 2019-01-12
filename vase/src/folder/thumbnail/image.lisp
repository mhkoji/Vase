(in-package :vase.folder.thumbnail)

(defmethod thumbnail-id ((th vase.image:image))
  (vase.image:image-id th))

(defmethod bulk-load ((repos vase.image:repository)
                      (thumbnail-ids list))
  (vase.image:bulk-load-by-ids repos thumbnail-ids))

(defmethod bulk-delete ((repos vase.image:repository)
                        (thumbnail-ids list))
  (vase.image:bulk-delete repos thumbnail-ids))
