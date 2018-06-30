;;; The representation of each content in a folder
;;; A folder does not contain its contents in memory because the number of the contents in the folder can be large.
;;; Thus the folder remains the contents in the database and fetch some of them if needed by use cases.
(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))

@export
(defun folder-contents (folder folder-repository &key from size)
  (let ((folder-dao (folder-repository-folder-dao folder-repository)))
    (let ((content-ids
           (-> (folder-content-select-ids folder-dao (folder-id folder))
               (safe-subseq from size))))
      (mapcar #'make-content content-ids))))


@export
(defgeneric update-contents (folder-repository op))

(defstruct appending
  "The object that represents the action of appending contents to a folder"
  folder-id contents)
(export 'make-appending)

(defmethod update-contents (folder-repository (op appending))
  (make-folder-repository
   :folder-dao
   (-> (folder-repository-folder-dao folder-repository)
       (folder-content-insert
        (appending-folder-id op)
        (mapcar #'content-id (appending-contents op))))))

(defstruct appending-bulk
  "The object that represents the action of bulk appending contents to a folder"
  appendings)
(export 'make-appending-bulk)

(defmethod update-contents (folder-repository (op appending-bulk))
  (dolist (appending (appending-bulk-appendings op))
    (update-contents folder-repository appending))
  folder-repository)


(defstruct moving
  "The object that represents the action of moving contents from a folder to another folder"
  source target contents)
(export 'make-moving)

(defmethod update-contents (folder-repository (op moving))
  (let* ((folder-dao
          (folder-repository-folder-dao folder-repository))
         (content-ids
          (mapcar #'content-id (moving-contents op)))
         (source-content-ids
          (set-difference (folder-content-select-ids
                           folder-dao
                           (folder-id (moving-source op)))
                          content-ids))
         (target-content-ids
          (union (folder-content-select-ids folder-dao
                                            (folder-id (moving-target op)))
                 content-ids)))
    (make-folder-repository
     :folder-dao
     (-> folder-dao
         (folder-content-delete
          (list (folder-id (moving-source op))))
         (folder-content-insert
          (folder-id (moving-source op)) source-content-ids)
         (folder-content-delete
          (list (folder-id (moving-target op))))
         (folder-content-insert
          (folder-id (moving-target op)) target-content-ids)))))
