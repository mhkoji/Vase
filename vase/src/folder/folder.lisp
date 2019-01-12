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
           :make-content-repository

           :contents-appending
           :bulk-append-contents

           :bulk-save
           :bulk-delete

           :repository
           :make-repository
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
                :bulk-load))
(in-package :vase.folder)

(defclass folder ()
  ((id
    :initarg :id
    :reader folder-id)
   (name
    :initarg :name
    :reader folder-name)
   (thumbnail
    :initarg :thumbnail
    :reader folder-thumbnail)
   (modified-at
    :initarg :modified-at
    :reader folder-modified-at)))

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


;;; Content
(defmethod vase.folder.content:folder-id ((f folder))
  (folder-id f))

(defstruct content-repository db entity-repos)

(defun folder-contents (folder content-repos &key from size)
  (vase.folder.content:bulk-load-by-folder
   (content-repository-entity-repos content-repos)
   (content-repository-db content-repos)
   folder
   :from from
   :size size))

(defun contents-appending (folder contents)
  (vase.folder.content:make-appending :folder folder
                                      :contents contents))

(defun bulk-append-contents (db appendings)
  (vase.folder.content:bulk-append db appendings))
