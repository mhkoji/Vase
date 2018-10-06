(defpackage :vase.app.tag
  (:use :cl)
  (:import-from :cl-arrows :-> :->>)
  (:import-from :alexandria :when-let))
(in-package :vase.app.tag)

(defun tag->dto (tag)
  (list :id (vase.entities.tag:tag-id tag)
        :name (vase.entities.tag:tag-name tag)))

(defun create (db name)
  (let ((tag (vase.entities.tag.repository:make db name)))
    (vase.entities.tag.repository:save db tag))
  (values))
(export 'create)

(defun list-by-range (db from size)
  (->> (vase.entities.tag.repository:load-by-range db from size)
       (mapcar #'tag->dto)))
(export 'list-by-range)

(defun delete-by-id (db tag-id)
  (vase.entities.tag.repository:delete-bulk db (list tag-id))
  (values))
(export 'delete-by-id)

(defun change-name (db tag-id &key name)
  (when-let ((tag (car (vase.entities.tag.repository:load-by-ids
                        db
                        (list tag-id)))))
    (setf (vase.entities.tag:tag-name tag) name)
    (vase.entities.tag.repository:update db tag))
  (values))
(export 'change-name)
