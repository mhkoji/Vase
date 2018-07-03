(defpackage :cocoa.use-case.folder.append-contents
  (:use :cl))
(in-package :cocoa.use-case.folder.append-contents)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (folder-repos)
  (lambda (&key folder-id contents)
    (let ((appending (cocoa.folder:make-appending
                      :folder-id folder-id :contents contents)))
      (cocoa.folder:update-contents folder-repos appending))))


@export
(defun exec (prepare &key folder-id contents)
  (funcall prepare :folder-id folder-id :contents contents))
