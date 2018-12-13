(defpackage :vase.tag.repos
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :tag-contents
           :content-id
           :update
           :attach-tag
           :detach-tag
           :save
           :bulk-load-by-ids
           :bulk-load-by-range
           :bulk-load-by-content
           :bulk-load-contents
           :bulk-delete))
(in-package :vase.tag.repos)

(defgeneric content-id (content))

(defgeneric content-type (content))


(defun content->content-row (content)
  (vase.tag.repos.db:make-content-row
   :id (content-id content)
   :type (symbol-name (content-type content))))

(defmethod content-id ((r vase.tag.repos.db:content-row))
  (vase.tag.repos.db:content-row-id r))

(defmethod content-type ((r vase.tag.repos.db:content-row))
  (alexandria:make-keyword (vase.tag.repos.db:content-row-type r)))


(defclass tag ()
  ((id
    :initarg :id
    :reader tag-id)
   (name
    :initarg :name
    :accessor tag-name)))

(defun tag->row (tag)
  (vase.tag.repos.db:make-row
   :tag-id (tag-id tag)
   :name (tag-name tag)))

(defun row->tag (r)
  (make-instance 'tag
   :id (vase.tag.repos.db:row-tag-id r)
   :name (vase.tag.repos.db:row-name r)))


(defun update (db tag)
  (vase.tag.repos.db:update db (tag->row tag)))

(defun save (db name)
  (row->tag (vase.tag.repos.db:insert db name)))


(defun attach-tag (db tag content)
  (vase.tag.repos.db:content/insert db
                                    (content->content-row content)
                                    (list (tag-id tag)))
  (values))

(defun detach-tag (db tag content)
  (vase.tag.repos.db:content/delete db
                                    (content->content-row content)
                                    (list (tag-id tag)))
  (values))


(defun bulk-load-by-ids (db ids)
  (mapcar #'row->tag (vase.tag.repos.db:select-by-ids db ids)))

(defun bulk-load-by-range (db offset size)
  (mapcar #'row->tag (vase.tag.repos.db:select-by-range db offset size)))

(defun bulk-load-by-content (db content)
  (mapcar #'row->tag (vase.tag.repos.db:content/select-tags
                      db
                      (content->content-row content))))

(defun bulk-load-contents (db tag)
  (vase.tag.repos.db:content/select db (tag-id tag)))



(defun bulk-delete (db ids)
  ;; Delete the contents attached with tags
  (dolist (tag (bulk-load-by-ids db ids))
    (dolist (content (bulk-load-contents db tag))
      (detach-tag db tag content)))
  (vase.tag.repos.db:delete db ids))
