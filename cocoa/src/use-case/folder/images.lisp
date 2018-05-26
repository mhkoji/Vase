(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

(defun content->image-dto (content)
  (list :id (cocoa.use-case.folder.content:content->image-id content)))

;;; folder images
@export
(defun list-images (folder-id &key from size folder-repository)
  ;@type! folder-id !integer
  ;@type! from integer 0
  ;@type! size integer 100
  ;@type! folder-repository !folder-repository
  (ensure-integer! from 0)
  (ensure-integer! size 100)
  (let ((contents (cocoa.entity.folder.content:list-by-folder-id
                   folder-repository
                   folder-id)))
    (mapcar #'content->image-dto (safe-subseq contents from size))))
