;;; Tag
(in-package :cocoa.entity.tag)
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

(defclass tag ()
  ((row
    :initarg :row
    :reader tag-row)
   (dao
    :initarg :dao
    :reader tag-dao)))

(defun make-tag (tag-row dao)
  (make-instance 'tag :row tag-row :dao dao))

@export
(defun make-tag/name (dao name)
  (the tag-row (tag-insert dao name)))

@export
(defgeneric save-tag (dao tag))

(defmethod save-tag (dao (row tag-row))
  ;; Already saved when row was made
  dao)

(defmethod save-tag (dao (tag tag))
  (tag-update dao (tag-row tag))
  dao)

@export
(defgeneric tag-id (tag))

(defmethod tag-id ((tag tag))
  (tag-row-tag-id (tag-row tag)))

@export
(defgeneric tag-name (tag))

(defmethod tag-name ((tag tag))
  (tag-row-name (tag-row tag)))

@export
(defgeneric (setf tag-name) (name tag))

(defmethod (setf tag-name) ((name string) (tag tag))
  (setf (tag-row-name (tag-row tag)) name))

@export
(defgeneric tag-contents (tag))

(defmethod tag-contents ((tag tag))
  (with-accessors ((tag-id tag-id) (dao tag-dao)) tag
    (mapcar #'content-row->content
            (tag-content-select-contents dao tag-id))))

@export
(defun list-tags/range (dao offset size)
  (mapcar (lambda (row) (make-tag row dao))
          (tag-select/range dao offset size)))

@export
(defun list-tags/ids (dao ids)
  (mapcar (lambda (row) (make-tag row dao)) (tag-select/ids dao ids)))

@export
(defun list-tags/content (dao content)
  (mapcar (lambda (row) (make-tag row dao))
          (tag-content-select-tags dao (content->row content))))

@export
(defun attach-tag (tag  content)
  (with-accessors ((tag-id tag-id) (dao tag-dao)) tag
    (tag-content-insert dao (content->row content) (list tag-id))))

@export
(defun detach-tag (tag content)
  (with-accessors ((tag-id tag-id) (dao tag-dao)) tag
    (tag-content-delete dao (content->row content) (list tag-id))))

@export
(defun delete-tags/ids (dao ids)
  (dolist (tag (list-tags/ids dao ids))
    (let ((tag-ids (list (tag-id tag))))
      (dolist (content (tag-contents tag))
        (let ((content-row (content->row content)))
          (setq dao (tag-content-delete dao content-row tag-ids))))))
  (setq dao (tag-delete dao ids)))
