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

(defmethod list-by-folder-id ((dao dao) folder-id)
  (mapcar #'id->content (folder-content-select-ids dao folder-id)))
