(defpackage :cocoa.use-case.folder.get-by-id
  (:use :cl)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.use-case.folder.get-by-id)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (folder-repos)
  (lambda (id)
    (->> (car (cocoa.folder:load-folders-by-ids folder-repos (list id)))
         cocoa.use-case.folder:response)))

@export
(defun exec (prepare id)
  (funcall prepare id))
