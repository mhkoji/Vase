(in-package :vase.folder.content)

;;; Image content
(defmethod content-type ((c vase.image:image))
  :image)

(defmethod content-entity-id ((c vase.image:image))
  (vase.image:image-id c))

(defmethod bulk-load ((repos vase.image:repository)
                      (type (eql :image))
                      (entity-ids list))
  (vase.image:bulk-load-by-ids repos entity-ids))


;;; Folder content
(defmethod content-type ((c vase.folder:folder))
  :folder)

(defmethod content-entity-id ((c vase.folder:folder))
  (vase.folder:folder-id c))

(defmethod bulk-load ((repos vase.folder:repository)
                      (type (eql :folder))
                      (entity-ids list))
  (vase.folder:bulk-load-by-ids repos entity-ids))



(defmethod bulk-load ((repos hash-table) (type symbol) (entity-ids list))
  (bulk-load (gethash type repos) type entity-ids))
