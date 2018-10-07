(defpackage :vase.api
  (:use :cl))
(in-package :vase.api)

(defun GET:/folders (offset size callback)
  (#j:vase:infra:api:folder:$list_by_range offset size (lambda (xs)
    (funcall callback
             (mapcar (lambda (x)
                       (vase.entities.folder:make-folder
                        :id (oget x "folder-id")
                        :name (oget x "name")
                        :thumbnail (vase.entities.folder:make-thumbnail
                                    :image-id
                                    (oget (oget x "thumbnail") "image-id")
                                    :url
                                    (oget (oget x "thumbnail") "url"))))
                     xs)))))

(defun GET:/folder/tags (folder-id callback)
  )

(defun PUT:/folder/tags (folder-id tags callback)
  )

(defun GET:tags (callback)
  )
