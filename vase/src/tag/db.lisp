(defpackage :vase.tag.db
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :tag-contents
           :content-id
           :update
           :attach-tag
           :detach-tag
           :create-tag
           :save
           :bulk-load-by-ids
           :list-ids-by-range
           :list-ids-by-content
           :bulk-delete))
(in-package :vase.tag.db)

(defgeneric content-id (content))

(defgeneric content-type (content))

(defun content->content-row (content)
  (vase.db.tag:make-content-row
   :id (content-id content)
   :type (symbol-name (content-type content))))


(defclass tag ()
  ((row
    :initarg :row
    :reader tag-row)))

(defun tag-id (tag)
  (vase.db.tag:row-tag-id (tag-row tag)))

(defun tag-name (tag)
  (vase.db.tag:row-name (tag-row tag)))

(defun (setf tag-name) (new-name tag)
  (setf (vase.db.tag:row-name (tag-row tag)) new-name))

(defun tag-contents (db content-repos tag)
  (let ((content-rows (vase.db.tag:content/select db (tag-id tag))))
    (let ((local-ids nil)
          (type->local-ids (make-hash-table))
          (local-id->content-row (make-hash-table))
          (local-id->content (make-hash-table :test #'equal)))
    (loop for local-id from 0
          for content-row in content-rows
           do (progn
                (push local-id local-ids)
                (setf (gethash local-id local-id->content-row)
                      content-row)))
    (loop for local-id in local-ids
          for content-row in content-rows
          do (let ((type (vase.db.tag:content-row-type content-row)))
               (push local-id (gethash type type->local-ids))))
    (loop for type being the hash-keys of type->local-ids
          for local-ids = (gethash type type->local-ids)
          for content-ids = (mapcar
                             (lambda (local-id)
                               (vase.db.tag:content-row-id
                                (gethash local-id local-id->content-row)))
                             local-ids)
          do (loop for local-id in local-ids
                   for content in (vase.tag.contents.repos:bulk-load
                                   content-repos
                                   (alexandria:make-keyword type)
                                   content-ids)
                   do (setf (gethash local-id local-id->content) content)))
    (mapcar (lambda (local-id)
              (gethash local-id local-id->content))
            local-ids))))

(defun update (db tag)
  (let ((row (tag-row tag)))
    (vase.db.tag:delete db (list (vase.db.tag:row-tag-id row)))
    (vase.db.tag:insert db (vase.db.tag:row-name row))))

(defun attach-tag (db tag content)
  (vase.db.tag:content/insert db
                              (content->content-row content)
                              (list (tag-id tag))))

(defun detach-tag (db tag content)
  (vase.db.tag:content/delete db
                              (content->content-row content)
                              (list (tag-id tag))))

(defun save (db name)
  (let ((row (vase.db.tag:insert db name)))
    (make-instance 'tag :row row)))


(defun bulk-load-by-ids (db make-instance ids)
  (mapcar (lambda (row) (funcall make-instance :row row))
          (vase.db.tag:select-by-ids db ids)))

(defun list-ids-by-range (db offset size)
  (mapcar #'vase.db.tag:row-tag-id
          (vase.db.tag:select-by-range db offset size)))

(defun list-ids-by-content (db content)
  (mapcar #'vase.db.tag:row-tag-id
          (vase.db.tag:content/select-tags
           db
           (content->content-row content))))

(defun bulk-delete (db ids)
  ;; Delete the contents attached with tags
  (dolist (row (vase.db.tag:select-by-ids db ids))
    (let ((tag-id (vase.db.tag:row-tag-id row)))
      (dolist (content-row (vase.db.tag:content/select db tag-id))
        (setq db (vase.db.tag:content/delete
                  db
                  content-row
                  (list tag-id))))))
  (vase.db.tag:delete db ids))
