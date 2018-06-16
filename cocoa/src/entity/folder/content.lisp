;;; The representation of each content in a folder
;;; A folder does not contain its contents in memory because the number of the contents in the folder can be large.
;;; Thus the folder remains the contents in the database and fetch some of them if needed by use cases.
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

@export
(defgeneric content-id (content)
  (:documentation "Returns the unique id of a content"))

(defclass folder-content ()
  ((content-id :initarg :content-id
               :reader content-id)))


(defun id->content (id)
  (make-instance 'folder-content :content-id id))

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))

@export
(defun folder-contents (dao folder &key from size)
  (let ((contents (folder-content-select-ids dao (folder-id folder))))
    (mapcar #'id->content (safe-subseq contents from size))))


@export
(defgeneric update-contents (dao op))


(defstruct appending
  "The object that represents the action of appending contents to a folder"
  folder-id contents)
(export 'make-appending)

(defmethod update-contents (dao (op appending))
  (let ((folder-id (appending-folder-id op))
        (content-ids (mapcar #'content-id (appending-contents op))))
    (folder-content-insert dao folder-id content-ids)))


(defstruct appending-bulk
  "The object that represents the action of bulk appending contents to a folder"
  appendings)
(export 'make-appending-bulk)

(defmethod update-contents (dao (op appending-bulk))
  (dolist (appending (appending-bulk-appendings op))
    (update-contents dao appending)))


(defstruct moving
  "The object that represents the action of moving contents from a folder to another folder"
  source target contents)
(export 'make-moving)

(defmethod update-contents (dao (op moving))
  (let* ((content-ids
          (mapcar #'content-id (moving-contents op)))
         (source-content-ids
          (set-difference
           (folder-content-select-ids
            dao (folder-id (moving-source op)))
           content-ids))
         (target-content-ids
          (union
           (folder-content-select-ids
            dao (folder-id (moving-target op)))
           content-ids)))
    (-> dao
        (folder-content-delete
         (list (folder-id (moving-source op))))
        (folder-content-insert
         (folder-id (moving-source op)) source-content-ids)
        (folder-content-delete
         (list (folder-id (moving-target op))))
        (folder-content-insert
         (folder-id (moving-target op)) target-content-ids))))
