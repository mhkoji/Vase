(in-package :vase.tag.contents)

(defmethod content-id ((c vase.folder:folder))
  (vase.folder:folder-id c))

(defmethod content-type ((c vase.folder:folder))
  :folder)

(defmethod bulk-load ((repos vase.folder:repository)
                      (type (eql :folder)) (content-ids list))
  (vase.folder:bulk-load-by-ids repos content-ids))
