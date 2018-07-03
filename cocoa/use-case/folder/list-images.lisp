(defpackage :cocoa.use-case.folder.list-images
  (:use :cl))
(in-package :cocoa.use-case.folder.list-images)
(cl-annot:enable-annot-syntax)

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

@export
(defun prepare (folder-repos)
  ;@type! folder-repos !folder-repository
  (lambda (folder-id &key from size)
    "The use case of listing images in a folder"
    ;@type! folder-id !integer
    ;@type! from integer 0
    ;@type! size integer 100
    (ensure-integer! from 0)
    (ensure-integer! size 100)
    (let ((folder (car (cocoa.folder:load-folders-by-ids
                        folder-repos
                        (list folder-id)))))
      (mapcar #'cocoa.use-case.folder:content->image-dto
              (cocoa.folder:folder-contents
               folder folder-repos :from from :size size)))))


@export
(defun exec (prepare folder-id &key from size)
  (funcall prepare folder-id :from from :size size))
