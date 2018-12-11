(defpackage :vase.folder
  (:use :cl)
  (:export :thumbnail
           :thumbnail-id
           :content
           :content-type
           :content-entity-id
           :folder
           :folder-id
           :folder-name
           :folder-thumbnail
           :make-source
           :bulk-add)
  (:import-from :vase.folder.repos
                :thumbnail-id
                :folder
                :folder-id
                :folder-name
                :folder-thumbnail)
  (:import-from :vase.folder.content.repos
                :content-type
                :content-entity-id))
(in-package :vase.folder)

;;; Thumbnail
(defclass thumbnail ()
  ((get-id :initarg :get-id
           :reader get-id)))

(defmethod thumbnail-id ((thumbnail thumbnail))
  (funcall (get-id thumbnail) thumbnail))


;;; Content
(defclass content ()
  ((type :initarg :type
         :type keyword
         :reader content-type)
   (get-entity-id :initarg :get-entity-id
                  :reader get-entity-id)))

(defmethod content-entity-id ((c content))
  (funcall (get-entity-id c) c))

(defmethod vase.folder.content.repos:folder-id ((f folder))
  (folder-id f))


(defstruct source name thumbnail modified-at)

(defun bulk-add (id-generator db sources)
  (let ((folder-ids (mapcar (lambda (s)
                              (vase.id:gen id-generator
                                           (source-name s)))
                            sources)))
    ;; Delete existing folders if any
    (vase.folder.repos:bulk-delete db folder-ids)
    (let ((folders (mapcar (lambda (folder-id s)
                             (make-instance 'folder
                              :id folder-id
                              :name (source-name s)
                              :thumbnail (source-thumbnail s)
                              :modified-at (source-modified-at s)))
                           folder-ids sources)))
      (vase.folder.repos:bulk-save db folders)
      folders)))
