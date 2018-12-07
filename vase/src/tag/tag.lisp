(defpackage :vase.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :tag-contents
           :content
           :content-id
           :content-type
           :update
           :save
           :repository
           :make-repository
           :bulk-load-by-ids
           :bulk-load-by-range
           :bulk-load-by-content
           :bulk-delete
           :attach-tag
           :detach-tag
           :create-tag
           :set-tags)
  (:import-from :vase.tag.db
                :tag-id
                :tag-name
                :content-id
                :content-type))
(in-package :vase.tag)

;;;; The definition of a content, to which tags are attached
(defclass content ()
  ((get-id
    :initarg :get-id
    :reader get-id)
   (type
    :initarg :type
    :type :keyword
    :reader content-type)))

(defmethod content-id ((c content))
  (funcall (get-id c) c))


(defclass tag (vase.tag.db:tag)
  ((db
    :initarg :db
    :reader tag-db)
   (content-repos
    :initarg :content-repos
    :reader tag-content-repos)))

(defun tag-contents (tag)
  (vase.tag.db:tag-contents (tag-db tag) (tag-content-repos tag) tag))

(defun update (tag)
  (vase.tag.db:update (tag-db tag) tag)
  (values))


(defstruct repository db content-repos)

(defun save (repos name)
  (let ((row (vase.tag.db:save (repository-db repos) name)))
    (change-class row 'tag
                  :db (repository-db repos)
                  :content-repos (repository-content-repos repos))))

(defun bulk-load-by-ids (repos ids)
  (let ((db (repository-db repos))
        (content-repos (repository-content-repos repos)))
    (let ((make-instance (lambda (&rest args)
                           (apply #'make-instance 'tag
                                  :db db
                                  :content-repos content-repos
                                  args))))
      (vase.tag.db:bulk-load-by-ids db make-instance ids))))

(defun bulk-load-by-range (repos offset size)
  (let ((db (repository-db repos)))
    (let ((ids (vase.tag.db:list-ids-by-range db offset size)))
      (bulk-load-by-ids repos ids))))

(defun bulk-load-by-content (repos content)
  (let ((db (repository-db repos)))
    (let ((ids (vase.tag.db:list-ids-by-content db content)))
      (bulk-load-by-ids repos ids))))

(defun bulk-delete (repos ids)
  (vase.tag.db:bulk-delete (repository-db repos) ids)
  (values))


(defun attach-tag (tag content)
  (vase.tag.db:attach-tag (tag-db tag) tag content))

(defun detach-tag (tag content)
  (vase.tag.db:detach-tag (tag-db tag) tag content))

(defun set-tags (repos content ids)
  (dolist (tag (bulk-load-by-content repos content))
    (detach-tag tag content))
  (dolist (tag (bulk-load-by-ids repos ids))
    (attach-tag tag content)))
