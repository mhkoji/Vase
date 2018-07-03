(defpackage :cocoa.use-case.folder.list-by-ids
  (:use :cl)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.use-case.folder.list-by-ids)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (folder-repos)
  (lambda (ids)
    (->> (cocoa.folder:load-folders-by-ids folder-repos ids)
         (mapcar #'cocoa.use-case.folder:response))))

@export
(defun exec (prepare ids)
  (funcall prepare ids))
