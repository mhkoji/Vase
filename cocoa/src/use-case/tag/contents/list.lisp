(defpackage :cocoa.use-case.tag.contents
  (:use :cl :cocoa.entity.tag))
(in-package :cocoa.use-case.tag.contents)
(cl-annot:enable-annot-syntax)

(defun tag-typed-contents (tag type-content-ids->object)
  (let ((contents (tag-contents tag))
        (local-ids nil)
        (type->local-ids (make-hash-table))
        (id->content (make-hash-table))
        (id->converted (make-hash-table :test #'equal)))
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
                               (content-id
                                (gethash local-id id->content)))
                             local-ids) do
      (loop for local-id in local-ids
            for converted-obj in (funcall type-content-ids->object
                                          type content-ids) do
        (progn
          (setf (gethash local-id id->converted) converted-obj))))
    (mapcar (alexandria:rcurry #'gethash id->converted) local-ids)))

@export
(defgeneric list-typed-contents (container type content-ids))

@export
(defun list-by-id (tag-dao container)
  (let ((type-content-ids->object
         (alexandria:curry #'list-typed-contents container)))
    (lambda (tag-id)
      (let ((tag (car (list-tags/ids tag-dao (list tag-id)))))
        (tag-typed-contents tag type-content-ids->object)))))
