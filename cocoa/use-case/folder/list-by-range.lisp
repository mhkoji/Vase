(defpackage :cocoa.use-case.folder.list-by-range
  (:use :cl)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.use-case.folder.list-by-range)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (folder-repos)
  (lambda (from size)
    (->> (cocoa.folder:load-folders-by-range folder-repos from size)
         (mapcar #'cocoa.use-case.folder:response))))

@export
(defun exec (prepare &key from size)
  (funcall prepare from size))
