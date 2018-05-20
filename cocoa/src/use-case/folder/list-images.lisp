(defpackage :cocoa.use-case.folder.list-images
  (:use :cl
        :cocoa.entity.folder
        :cocoa.use-case.folder.inject))
(in-package :cocoa.use-case.folder.list-images)
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

;;; folder images
@export
(defun execute (folder-id &key from size folder-repository)
  ;@type! folder-id !integer
  ;@type! from integer 0
  ;@type! size integer 100
  ;@type! folder-repository !folder-repository
  (ensure-integer! from 0)
  (ensure-integer! size 100)
  (let ((folder (car (list-folders/ids folder-repository
                                       (make-list-spec)
                                       (list folder-id)))))
    (mapcar (lambda (content)
              (list :id (content->image-id content)))
            (safe-subseq (folder-contents folder) from size))))
