(defpackage :vase.entities.folder.content.repository
  (:use :cl
        :vase.entities.folder
        :vase.entities.folder.content
        :vase.entities.folder.content.op
        :vase.entities.folder.content.db)
  (:import-from :cl-arrows :->))
(in-package :vase.entities.folder.content.repository)
(cl-annot:enable-annot-syntax)

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))


;;; A folder does not contain its contents in memory because the number of the contents in the folder can be large.
;;; Thus the folder remains the contents in the database and fetch some of them if needed by use cases."
@export
(defun folder-contents (db folder &key from size)
  (let ((content-ids
         (-> (folder-content-select-ids db (folder-id folder))
             (safe-subseq from size))))
    (mapcar #'make-content content-ids)))

@export
(defgeneric update (db op))

(defmethod update (db (op deleting-bulk))
  (let ((folder-ids (mapcar #'vase.entities.folder:folder-id
                            (deleting-bulk-folders op))))
    (folder-content-delete db folder-ids)))

(defmethod update (db (op appending))
  (let ((folder-id (folder-id (appending-folder op)))
        (content-ids (mapcar #'content-id (appending-contents op))))
    (folder-content-insert db folder-id content-ids)))

(defmethod update (db (op appending-bulk))
  (dolist (appending (appending-bulk-appendings op))
    (update db appending))
  db)

#+nil
(defmethod update (db (op moving))
  (let* ((content-ids
          (mapcar #'content-id (moving-contents op)))
         (source-content-ids
          (set-difference (folder-content-select-ids
                           db
                           (folder-id (moving-source op)))
                          content-ids))
         (target-content-ids
          (union (folder-content-select-ids db
                                            (folder-id (moving-target op)))
                 content-ids)))
    (-> db
        (folder-content-delete
         (list (folder-id (moving-source op))))
        (folder-content-insert
         (folder-id (moving-source op)) source-content-ids)
        (folder-content-delete
         (list (folder-id (moving-target op))))
        (folder-content-insert
         (folder-id (moving-target op)) target-content-ids))))
