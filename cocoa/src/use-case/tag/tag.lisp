(defpackage :cocoa.use-case.tag
  (:use :cl)
  (:import-from :cl-arrows :-> :->>)
  (:import-from :alexandria :when-let))
(in-package :cocoa.use-case.tag)
(cl-annot:enable-annot-syntax)

(defun tag->dto (tag)
  (list :id (cocoa.tag:tag-id tag) :name (cocoa.tag:tag-name tag)))

@export
(defun create (name &key tag-repository)
  (-> (cocoa.tag:make-tag tag-repository name)
      (cocoa.tag:save-tag tag-repository))
  (values))

@export
(defun list-by-range (from size &key tag-repository)
  (->> (cocoa.tag:load-tags-by-range tag-repository from size)
       (mapcar #'tag->dto)))

@export
(defun delete-by-id (tag-id &key tag-repository)
  (cocoa.tag:delete-tags tag-repository (list tag-id))
  (values))

@export
(defun change-name (tag-id name &key tag-repository)
  (when-let ((tag (car (cocoa.tag:load-tags-by-ids
                        tag-repository
                        (list tag-id)))))
    (setf (cocoa.tag:tag-name tag) name)
    (cocoa.tag:save-tag tag-repository tag))
  (values))
