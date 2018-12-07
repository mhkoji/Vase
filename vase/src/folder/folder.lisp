(defpackage :vase.folder
  (:use :cl)
  (:export :thumbnail
           :content
           :content-type
           :content-entity-id
           :folder
           :folder-id
           :folder-name
           :folder-thumbnail
           :folder-contents
           :bulk-append-contents
           :make-factory
           :repository
           :make-repository
           :bulk-load-by-ids
           :load-by-id
           :bulk-load-by-range
           :bulk-load-by-search
           :make-source
           :bulk-save)
  (:import-from :vase.folder.db
                :folder-id
                :folder-name
                :folder-thumbnail
                :folder-modified-at
                :thumbnail-id))
(in-package :vase.folder)

;;; Thumbnail
(defclass thumbnail ()
  ((get-id :initarg :get-id
           :reader get-id)))

(defmethod thumbnail-id ((thumbnail thumbnail))
  (funcall (get-id thumbnail) thumbnail))

;;; Folder
(defstruct factory id-generator)


(defstruct repository db thumbnail-repos content-repos)


(defclass folder (vase.folder.db:folder)
  ((db
    :initarg :db
    :reader folder-db)
   (content-repos
    :initarg :content-repos
    :reader folder-content-repos)))


(defun folder-contents (folder &key from size)
  (vase.folder.content.db:bulk-load (folder-db folder)
                                    (folder-content-repos folder)
                                    (folder-id folder)
                                    :from from
                                    :size size))

(defun bulk-append-contents (repos appendings)
  (vase.folder.content.db:bulk-append
   (repository-db repos)
   appendings))

(defun bulk-load-by-ids (repos ids)
  (let ((db (repository-db repos))
        (content-repos (repository-content-repos repos))
        (thumbnail-repos (repository-thumbnail-repos repos)))
    (let ((make-instance (lambda (&rest args)
                           (apply #'make-instance 'folder
                                  :db db
                                  :content-repos content-repos
                                  args))))
      (vase.folder.db:bulk-load-by-ids db thumbnail-repos make-instance
                                       ids))))


(defun load-by-id (repos folder-id)
  (car (bulk-load-by-ids repos (list folder-id))))


(defun bulk-load-by-range (repos offset size)
  (bulk-load-by-ids repos (vase.folder.db:list-ids-by-range
                           (repository-db repos) offset size)))


(defun bulk-load-by-search (repos name)
  (bulk-load-by-ids repos (vase.folder.db:list-ids-by-searching
                           (repository-db repos) name)))


(defstruct source name thumbnail modified-at)

(defun bulk-save (repos factory sources)
  (let ((db (repository-db repos))
        (id-generator (factory-id-generator factory)))
    (let ((folder-ids (mapcar (lambda (s)
                                (vase.id:gen id-generator
                                             (source-name s)))
                              sources)))
      ;; Delete existing folders if any
      (vase.folder.db:bulk-delete db folder-ids)
      (let ((folders (mapcar (lambda (folder-id s)
                               (make-instance 'vase.folder.db:folder
                                :id folder-id
                                :name (source-name s)
                                :thumbnail (source-thumbnail s)
                                :modified-at (source-modified-at s)))
                             folder-ids sources)))
        (vase.folder.db:bulk-save db folders))
      folder-ids)))
