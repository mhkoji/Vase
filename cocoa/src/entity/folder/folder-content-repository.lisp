;;; A representation of each content in a folder
;;; A folder does not contain its contents in memory because the number of the contents in the folder can be large.
;;; Thus the folder remains the contents in the database and fetch some of them if needed by use cases.
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))

@export
(defun folder-content-repository (dao)
  (make-repository :dao dao))

@export
(defun folder-contents (folder-content-repository folder
                        &key from size)
  (let ((dao (repository-dao folder-content-repository)))
    (let ((content-ids (-> (folder-content-select-ids
                            dao
                            (folder-id folder))
                           (safe-subseq from size))))
      (mapcar #'make-content content-ids))))


@export
(defgeneric update-folder-contents (folder-content-repository op))

(defstruct appending
  "The object that represents the action of appending contents to a folder"
  folder contents)
(export 'make-appending)

(defmethod update-folder-contents (folder-content-repository
                                   (op appending))
  (make-repository
   :dao
   (-> (repository-dao folder-content-repository)
       (folder-content-insert
        (folder-id (appending-folder op))
        (mapcar #'content-id (appending-contents op))))))

(defstruct appending-bulk
  "The object that represents the action of bulk appending contents to a folder"
  appendings)
(export 'make-appending-bulk)

(defmethod update-folder-contents (folder-content-repository
                                   (op appending-bulk))
  (dolist (appending (appending-bulk-appendings op))
    (update-folder-contents folder-content-repository appending))
  folder-content-repository)


(defstruct moving
  "The object that represents the action of moving contents from a folder to another folder"
  source target contents)
(export 'make-moving)

(defmethod update-folder-contents (folder-content-repository (op moving))
  (let* ((dao
          (repository-dao folder-content-repository))
         (content-ids
          (mapcar #'content-id (moving-contents op)))
         (source-content-ids
          (set-difference (folder-content-select-ids
                           dao
                           (folder-id (moving-source op)))
                          content-ids))
         (target-content-ids
          (union (folder-content-select-ids dao
                                            (folder-id (moving-target op)))
                 content-ids)))
    (make-repository
     :dao
     (-> dao
         (folder-content-delete
          (list (folder-id (moving-source op))))
         (folder-content-insert
          (folder-id (moving-source op)) source-content-ids)
         (folder-content-delete
          (list (folder-id (moving-target op))))
         (folder-content-insert
          (folder-id (moving-target op)) target-content-ids)))))
