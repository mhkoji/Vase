(defpackage :cocoa.tag
  (:use :cl)
  (:import-from :cl-arrows :-> :->>)
  (:import-from :alexandria :when-let))
(in-package :cocoa.tag)
(cl-annot:enable-annot-syntax)

(defun tag->dto (tag)
  (list :id (cocoa.entity.tag:tag-id tag)
        :name (cocoa.entity.tag:tag-name tag)))

@export
(defun create (name &key tag-repository)
  (-> (cocoa.entity.tag:make-tag tag-repository name)
      (cocoa.entity.tag:save-tag tag-repository))
  (values))

@export
(defun list-by-range (from size &key tag-repository)
  (->> (cocoa.entity.tag:load-tags-by-range tag-repository from size)
       (mapcar #'tag->dto)))

@export
(defun delete-by-id (tag-id &key tag-repository)
  (cocoa.entity.tag:delete-tags tag-repository (list tag-id))
  (values))

@export
(defun change-name (tag-id name &key tag-repository)
  (when-let ((tag (car (cocoa.entity.tag:load-tags-by-ids
                        tag-repository
                        (list tag-id)))))
    (setf (cocoa.entity.tag:tag-name tag) name)
    (cocoa.entity.tag:save-tag tag-repository tag))
  (values))
