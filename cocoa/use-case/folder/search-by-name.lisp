(defpackage :cocoa.use-case.folder.search-by-name
  (:use :cl)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.use-case.folder.search-by-name)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (folder-repos)
  (lambda (name)
    (->> (cocoa.folder:search-folders-by-name folder-repos name)
         (mapcar #'cocoa.use-case.folder:response))))

@export
(defun exec (prepare name)
  (funcall prepare name))

