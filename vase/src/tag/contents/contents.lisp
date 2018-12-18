(defpackage :vase.tag.contents
  (:use :cl)
  (:import-from :vase.tag
                :content-id
                :content-type)
  (:import-from :vase.tag.contents.repos
                :bulk-load))
(in-package :vase.tag.contents)

(defmethod content-id ((c vase.folder:folder))
  (vase.folder:folder-id c))

(defmethod content-type ((c vase.folder:folder))
  :folder)

(defmethod bulk-load ((repos vase.folder:repository)
                      (type (eql :folder)) (content-ids list))
  (vase.folder.repos:bulk-load-by-ids repos content-ids))
