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


(defstruct content-query folder-id from size)

@export
(defun add-contents (dao folder-id contents)
  (folder-content-insert dao folder-id (mapcar #'content-id contents)))

@export
(defun folder-content-query (folder &key from size)
  (make-content-query :folder-id (folder-id folder) :from from :size size))

(defclass simple-content ()
  ((content-id
    :initarg :content-id
    :reader content-id)))

(defun id->content (id)
  (make-instance 'simple-content :content-id id))

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))

@export
(defun list-contents-by-query (dao content-query)
  (mapcar #'id->content
          (safe-subseq (folder-content-select-ids dao
                        (content-query-folder-id content-query))
                       (content-query-from content-query)
                       (content-query-size content-query))))
