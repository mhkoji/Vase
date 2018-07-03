(defpackage :cocoa.use-case.folder.change-thumbnail
  (:use :cl))
(in-package :cocoa.use-case.folder.change-thumbnail)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (folder-repos)
  (lambda (folder-id image-id)
    (let ((folder (car (cocoa.folder:load-folders-by-ids
                        folder-repos
                        (list folder-id)))))
      (setf (cocoa.folder:folder-thumbnail folder)
            (cocoa.use-case.folder.thumbnail:make-of-image image-id))
      (cocoa.folder:update-folder folder-repos folder))))

@export
(defun exec (prepare folder-id image-id)
  (funcall prepare folder-id image-id))
