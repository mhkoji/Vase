(defpackage :cocoa.infra.db.folder.content.dao
  (:use :cl
        :cocoa.infra.db.folder.dao
        :cocoa.entity.folder.content))
(in-package :cocoa.infra.db.folder.content.dao)

;;; content
(defmethod add-by-folder-id ((dao dao) folder-id (contents list))
  (folder-content-insert dao folder-id (mapcar #'content-id contents)))


(defclass simple-content ()
  ((content-id
    :initarg :content-id
    :reader content-id)))

(defun id->content (id)
  (make-instance 'simple-content :content-id id))

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))

(defmethod list-by-query ((dao dao) (query content-query))
  (mapcar #'id->content
          (safe-subseq (let ((folder-id (content-query-folder-id query)))
                         (folder-content-select-ids dao folder-id))
                       (content-query-from query)
                       (content-query-size query))))
