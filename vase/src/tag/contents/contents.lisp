(defpackage :vase.tag.contents
  (:use :cl)
  (:export :folder
           :from-folder
           :folder-repos)
  (:import-from :vase.tag.contents.repos
                :bulk-load))
(in-package :vase.tag.contents)

(defclass folder (vase.tag:content
                  vase.folder:folder) ())

(defun from-folder (folder)
  (assert (typep folder 'vase.folder:folder))
  (change-class folder 'folder
                :type :folder
                :get-id #'vase.folder:folder-id))


(defmethod bulk-load ((repos vase.folder:repository)
                      (type (eql :folder)) (content-ids list))
  (mapcar #'from-folder (vase.folder:bulk-load-by-ids repos content-ids)))
