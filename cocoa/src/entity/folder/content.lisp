;;; The representation of each content in a folder
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
(defun list-contents (dao folder &key from size)
  (let ((contents (folder-content-select-ids dao (folder-id folder))))
    (mapcar #'id->content (safe-subseq contents from size))))


@export
(defgeneric update-contents! (dao op))

(defstruct op update-dao!)

(defun op (update-dao!)
  (make-op :update-dao! update-dao!))

(defmethod update-contents! (dao (op op))
  (funcall (op-update-dao! op) dao))


(defstruct append-contents-op folder-id contents)

@export
(defun append-contents-op (folder-id contents)
  (make-append-contents-op :folder-id folder-id :contents contents))

(defmethod update-contents! (dao (op append-contents-op))
  (let ((folder-id (append-contents-op-folder-id op))
        (content-ids (mapcar #'content-id (append-contents-op-contents op))))
    (folder-content-insert dao folder-id content-ids)))


@export
(defun bulk-append-contents-op (append-contents-ops)
  (op (lambda (dao)
        (dolist (append-contents-op append-contents-ops)
          (setq dao (update-contents! dao append-contents-op)))
        dao)))


@export
(defun move-contents-op (source-folder target-folder contents)
  (let ((content-ids (mapcar #'content-id contents)))
    (op (lambda (dao)
          (let ((source-content-ids
                 (set-difference (folder-content-select-ids dao
                                  (folder-id source-folder))
                                 content-ids))
                (target-content-ids
                 (union (folder-content-select-ids dao
                         (folder-id target-folder))
                        content-ids)))
            (-> dao
                (folder-content-delete
                 (list (folder-id source-folder)))
                (folder-content-insert
                 (folder-id source-folder) source-content-ids)
                (folder-content-delete
                 (list (folder-id target-folder)))
                (folder-content-insert
                 (folder-id target-folder) target-content-ids)))))))
