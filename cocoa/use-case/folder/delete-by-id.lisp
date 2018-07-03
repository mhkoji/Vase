(defpackage :cocoa.use-case.folder.delete-by-id
  (:use :cl))
(in-package :cocoa.use-case.folder.delete-by-id)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (folder-repos)
  (lambda (folder-id)
    (cocoa.folder:delete-folders-by-ids folder-repos (list folder-id))))

@export
(defun exec (prepare folder-id)
  (funcall prepare folder-id))
