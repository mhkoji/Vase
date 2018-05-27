;;; The representation of each content in a folder
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

@export
(defgeneric content-id (content)
  (:documentation "Returns the unique id of a content"))


@export
(defgeneric folder-content-insert (dao folder-id content-id-list))
@export
(defgeneric folder-content-select-ids (dao folder-id))
@export
(defgeneric folder-content-delete (dao folder-id-list))


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

(defstruct content-query folder-id from size)

(defun list-contents-by-query (dao content-query)
  (mapcar #'id->content
          (safe-subseq (folder-content-select-ids dao
                        (content-query-folder-id content-query))
                       (content-query-from content-query)
                       (content-query-size content-query))))


@export
(defun folder-contents (folder &key from size)
  (let ((dao (slot-value folder 'dao))
        (query (make-content-query :folder-id (folder-id folder)
                                   :from from
                                   :size size)))
    (list-contents-by-query dao query)))

@export
(defun (setf folder-contents) (contents folder)
  (let ((dao (slot-value folder 'dao))
        (folder-id (folder-id folder))
        (content-ids (mapcar #'content-id contents)))
    (folder-content-insert dao folder-id content-ids)))
