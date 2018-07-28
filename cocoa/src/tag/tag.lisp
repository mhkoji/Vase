(defpackage :cocoa.tag
  (:use :cl)
  (:import-from :cl-arrows :-> :->>)
  (:import-from :alexandria :when-let))
(in-package :cocoa.tag)

(defun tag->dto (tag)
  (list :id (cocoa.entity.tag:tag-id tag)
        :name (cocoa.entity.tag:tag-name tag)))

(defun create (db name)
  (let ((tag (cocoa.entity.tag.repository:make db name)))
    (cocoa.entity.tag.repository:save db tag))
  (values))
(export 'create)

(defun list-by-range (db from size)
  (->> (cocoa.entity.tag.repository:load-by-range db from size)
       (mapcar #'tag->dto)))
(export 'list-by-range)

(defun delete-by-id (db tag-id)
  (cocoa.entity.tag.repository:delete-bulk db (list tag-id))
  (values))
(export 'delete-by-id)

(defun change-name (db tag-id &key name)
  (when-let ((tag (car (cocoa.entity.tag.repository:load-by-ids
                        db
                        (list tag-id)))))
    (setf (cocoa.entity.tag:tag-name tag) name)
    (cocoa.entity.tag.repository:update db tag))
  (values))
(export 'change-name)
