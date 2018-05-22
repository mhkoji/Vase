(defpackage :cocoa.infra.db.tag.dao
  (:use :cl :cocoa.entity.tag))
(in-package :cocoa.infra.db.tag.dao)
(cl-annot:enable-annot-syntax)

(defstruct tag-row tag-id name)
(export 'make-tag-row)
(export 'tag-row)
(export 'tag-row-tag-id)
(export 'tag-row-name)

(defstruct content-row id type)
(export 'make-content-row)
(export 'content-row)
(export 'content-row-id)
(export 'content-row-type)

@export
(defclass dao () ())

@export
(defgeneric tag-insert (dao name))
@export
(defgeneric tag-delete (dao tag-id-list))
@export
(defgeneric tag-update (dao tag-row))
@export
(defgeneric tag-select/ids (dao ids))
@export
(defgeneric tag-select/range (dao offset size))

@export
(defgeneric tag-content-insert (dao content-row tag-id-list))
@export
(defgeneric tag-content-delete (dao content-row tag-id-list))
@export
(defgeneric tag-content-select-tags (dao content-row))
@export
(defgeneric tag-content-select-contents (dao tag-id))


(defclass simple-content ()
  ((id
    :initarg :id
    :reader content-id)
   (type
    :initarg :type
    :type :keyword
    :reader content-type)))

(defun make-simple-content (id type)
  (make-instance 'simple-content :id id :type type))

(defun content-row->content (row)
  (make-simple-content (content-row-id row)
                       (alexandria:make-keyword
                        (content-row-type row))))

(defun content->row (content)
  (make-content-row :id (content-id content)
                    :type (symbol-name
                           (content-type content))))

(defclass dao-tag ()
  ((tag-row
    :initarg :tag-row
    :reader dao-tag-tag-row)
   (dao
    :initarg :dao
    :reader dao-tag-dao)))

(defmethod make-tag/name ((factory dao) (name string))
  (the tag-row (tag-insert factory name)))

(defun make-dao-tag (tag-row dao)
  (make-instance 'dao-tag :tag-row tag-row :dao dao))

(defmethod save-tag ((dao dao) (tag tag-row))
  ;; Already saved
  (make-dao-tag tag dao))

(defmethod save-tag ((dao dao) (tag dao-tag))
  (tag-update dao (dao-tag-tag-row tag))
  tag)

(defmethod tag-id ((tag dao-tag))
  (tag-row-tag-id (dao-tag-tag-row tag)))

(defmethod tag-name ((tag dao-tag))
  (tag-row-name (dao-tag-tag-row tag)))

(defmethod (setf tag-name) ((name string) (tag dao-tag))
  (setf (tag-row-name (dao-tag-tag-row tag)) name))

(defmethod tag-contents ((tag dao-tag))
  (with-accessors ((dao dao-tag-dao)) tag
    (mapcar #'content-row->content
            (tag-content-select-contents dao (tag-id tag)))))

(defmethod list-tags/range ((dao dao) offset size)
  (mapcar (lambda (row) (make-dao-tag row dao))
          (tag-select/range dao offset size)))

(defmethod list-tags/ids ((dao dao) (ids list))
  (mapcar (lambda (row) (make-dao-tag row dao))
          (tag-select/ids dao ids)))

(defmethod list-tags/content ((dao dao) content)
  (mapcar (lambda (row)
            (make-dao-tag row dao))
          (tag-content-select-tags dao (content->row content))))

(defmethod attach-tag ((tag dao-tag) content)
  (with-accessors ((dao dao-tag-dao)) tag
    (tag-content-insert dao (content->row content) (list (tag-id tag)))))

(defmethod detach-tag ((tag dao-tag) content)
  (with-accessors ((dao dao-tag-dao)) tag
    (tag-content-delete dao (content->row content) (list (tag-id tag)))))

(defmethod delete-tags/ids ((dao dao) (ids list))
  (dolist (tag (list-tags/ids dao ids))
    (dolist (content (tag-contents tag))
      (tag-content-delete dao (content->row content) (list (tag-id tag)))))
  (tag-delete dao ids))
