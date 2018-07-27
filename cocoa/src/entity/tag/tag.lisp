(defpackage :cocoa.entity.tag
  (:use :cl)
  (:import-from :cl-arrows :->> :->))
(in-package :cocoa.entity.tag)

;;;; The definition of a content, to which tags are attached
(defclass content ()
  ((id
    :initarg :id
    :reader content-id)
   (type
    :initarg :type
    :type :keyword
    :reader content-type)))
(export '(content content-id content-type))

;;;; The definition of a tag
;;;; Generic functions are used becase some persistent system for the jobs of a tag should be hidden.
(defgeneric tag-id (tag))
(export 'tag-id)

(defgeneric tag-name (tag))
(export 'tag-name)

(defgeneric tag-contents (tag))
(export 'tag-contents)


(defgeneric render-contents (container type content-ids))
(export 'render-contents)

(defun load-rendered-contents-by-tag (tag container)
  (let ((contents (tag-contents tag))
        (local-ids nil)
        (type->local-ids (make-hash-table))
        (id->content (make-hash-table))
        (id->rendered (make-hash-table :test #'equal)))
    (loop for local-id from 0
          for content in contents do
      (progn
        (push local-id local-ids)
        (setf (gethash local-id id->content) content)))
    (loop for local-id in local-ids
          for content in contents do
      (let ((type (content-type content)))
        (push local-id (gethash type type->local-ids))))
    (loop for type being the hash-keys of type->local-ids
          for local-ids = (gethash type type->local-ids)
          for content-ids = (mapcar
                             (lambda (local-id)
                               (content-id (gethash local-id id->content)))
                             local-ids) do
      (loop for local-id in local-ids
            for rendered-obj in (render-contents container
                                                  type
                                                  content-ids) do
        (progn
          (setf (gethash local-id id->rendered) rendered-obj))))
    (mapcar (alexandria:rcurry #'gethash id->rendered) local-ids)))
(export 'load-rendered-contents-by-tag)
