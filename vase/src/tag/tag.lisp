(defpackage :vase.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :tag-contents

           :content
           :content-id
           :content-type
           :content-tags
           :set-content-tags

           :save
           :update
           :bulk-load-by-ids
           :bulk-load-by-range
           :bulk-load-contents
           :bulk-delete)
  (:import-from :vase.tag.repos
                :tag
                :tag-id
                :tag-name

                :content-id
                :content-type

                :save
                :update
                :bulk-load-by-ids
                :bulk-load-by-range
                :bulk-load-by-content
                :bulk-load-contents
                :bulk-delete))
(in-package :vase.tag)

(defun content-tags (content db)
  (vase.tag.repos:bulk-load-by-content db content))

(defun set-content-tags (db content tags)
  (dolist (tag (vase.tag.repos:bulk-load-by-content db content))
    (vase.tag.repos:detach-tag db tag content))
  (dolist (tag tags)
    (vase.tag.repos:attach-tag db tag content))
  (values))


(defun tag-contents (tag db content-repos)
  (let ((local-ids nil)
        (type->local-ids (make-hash-table))
        (local-id->content-id (make-hash-table :test #'equal))
        (local-id->content (make-hash-table :test #'equal)))
    (loop for local-id from 0
          for abstract-content in (vase.tag.repos:bulk-load-contents db tag)
          do (let ((type (content-type abstract-content))
                   (content-id (content-id abstract-content)))
               (push local-id local-ids)
               (push local-id (gethash type type->local-ids))
               (setf (gethash local-id local-id->content-id) content-id)))
    (loop for type being the hash-keys of type->local-ids
          for local-ids = (gethash type type->local-ids)
          for content-ids = (mapcar (lambda (local-id)
                                      (gethash local-id
                                               local-id->content-id))
                                    local-ids)
          for contents = (vase.tag.contents.repos:bulk-load content-repos
                                                            type
                                                            content-ids)
          do (loop for local-id in local-ids
                   for content in contents
                   do (setf (gethash local-id local-id->content) content)))
    (mapcar (lambda (local-id) (gethash local-id local-id->content))
            local-ids)))

