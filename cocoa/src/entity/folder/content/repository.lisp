;;; A representation of each content in a folder
;;; A folder does not contain its contents in memory because the number of the contents in the folder can be large.
;;; Thus the folder remains the contents in the database and fetch some of them if needed by use cases.
(defpackage :cocoa.entity.folder.content.repository
  (:use :cl
        :cocoa.entity.folder.content
        :cocoa.entity.folder.content.op))
(in-package :cocoa.entity.folder.content.repository)
(cl-annot:enable-annot-syntax)

@export
(defgeneric folder-content-insert (db folder-id content-id-list))
@export
(defgeneric folder-content-select-ids (db folder-id))
@export
(defgeneric folder-content-delete (db folder-id-list))

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))


@export
(defun folder-contents (db folder &key from size)
  (let ((content-ids (-> (folder-content-select-ids
                          db
                          (folder-id folder))
                         (safe-subseq from size))))
    (mapcar #'make-content content-ids)))

@export
(defgeneric update (db op))

(defmethod update (db (op deleteing-bulk))
  (let ((folder-ids (mapcar #'cocoa.entity.folder:folder-id
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
    (-> (folder-content-delete
         (list (folder-id (moving-source op))))
        (folder-content-insert
         (folder-id (moving-source op)) source-content-ids)
        (folder-content-delete
         (list (folder-id (moving-target op))))
        (folder-content-insert
         (folder-id (moving-target op)) target-content-ids))))
