(defpackage :cocoa.entity.tag.repository
  (:use :cl
        :cocoa.entity.tag
        :cocoa.entity.tag.db)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.entity.tag.repository)
(cl-annot:enable-annot-syntax)

(defun content-row-content (row)
  (make-instance 'content
   :id (content-row-id row)
   :type (alexandria:make-keyword (content-row-type row))))

(defun content-content-row (content)
  (make-content-row
   :id (content-id content)
   :type (symbol-name (content-type content))))

(defclass tag ()
  ((db :initarg :db :reader tag-db)
   (row :initarg :row :reader tag-row)))

(defmethod tag-id ((tag tag))
  (tag-row-tag-id (tag-row tag)))

(defmethod tag-name ((tag tag))
  (tag-row-name (tag-row tag)))

(defmethod (setf tag-name) (new-name (tag tag))
  (setf (tag-row-name (tag-row tag)) new-name))

(defmethod tag-contents ((tag tag))
  (->> (tag-content-select-contents (tag-db tag) (tag-id tag))
       (mapcar #'content-row-content)))

@export
(defun attach-tag (tag content)
  (with-accessors ((tag-id tag-id) (db tag-db)) tag
    (tag-content-insert db (content-content-row content) (list tag-id))))

@export
(defun detach-tag (tag content)
  (with-accessors ((tag-id tag-id) (db tag-db)) tag
    (tag-content-delete db (content-content-row content) (list tag-id))))


@export
(defun make (db name)
  (the tag-row (tag-insert db name)))

@export
(defun save (db tag-row)
  (declare (ignore tag-row))
  ;; Already saved when row was made
  db)

@export
(defun update (db tag)
  (tag-update db (tag-row tag)))

@export
(defun load-by-range (db offset size)
  (mapcar (lambda (row) (make-instance 'tag :db db :row row))
          (tag-select/range db offset size)))

@export
(defun load-by-ids (db ids)
  (mapcar (lambda (row) (make-instance 'tag :db db :row row))
          (tag-select/ids db ids)))

@export
(defun load-by-content (db content)
  (mapcar (lambda (row) (make-instance 'tag :row row :db db))
          (tag-content-select-tags db (content-content-row content))))

@export
(defun delete-bulk (db ids)
  (dolist (tag (load-by-ids db ids))
    (let ((tag-ids (list (tag-id tag))))
      (dolist (content (tag-contents tag))
        (let ((content-row (content-content-row content)))
          (setq db (tag-content-delete db content-row tag-ids))))))
  db)
