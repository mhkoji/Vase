(defpackage :vase.folder
  (:use :cl)
  (:export :thumbnail-id

           :content-type
           :content-entity-id

           :folder
           :folder-id
           :folder-name
           :folder-thumbnail
           :folder-contents

           :bulk-save
           :bulk-delete

           :repository
           :bulk-load-by-range
           :bulk-load-by-search
           :bulk-load-by-ids
           :load-by-id

           :make-source
           :bulk-create)
  (:import-from :vase.folder.content
                :content-type
                :content-entity-id)
  (:import-from :vase.folder.thumbnail
                :thumbnail-id
                :bulk-load)
  (:import-from :vase.folder.repos
                 :folder
                 :folder-id
                 :folder-name
                 :folder-thumbnail
                 :folder-modified-at

                 :bulk-save
                 :bulk-delete

                 :repository
                 :make-repository

                 :bulk-load-by-range
                 :bulk-load-by-search
                 :bulk-load-by-ids
                 :load-by-id))
(in-package :vase.folder)

;;; Content
(defmethod vase.folder.content.repos:folder-id ((f folder))
  (folder-id f))


(defun folder-contents (folder db content-repos &key from size)
  (vase.folder.content:bulk-load-by-folder content-repos db folder
                                           :from from
                                           :size size))


(defstruct source name thumbnail modified-at)

(defun bulk-create (id-generator sources)
  (let ((folder-ids (mapcar (lambda (s)
                              (vase.id:gen id-generator (source-name s)))
                            sources)))
    (mapcar (lambda (folder-id s)
              (make-instance 'folder
                             :id folder-id
                             :name (source-name s)
                             :thumbnail (source-thumbnail s)
                             :modified-at (source-modified-at s)))
            folder-ids sources)))
