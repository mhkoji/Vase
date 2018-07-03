(defpackage :cocoa.use-case.folder.add
  (:use :cl))
(in-package :cocoa.use-case.folder.add)
(cl-annot:enable-annot-syntax)

@export
(defun prepare (folder-repos name->folder-id)
  (lambda (&key name thumbnail)
    (let ((id (funcall name->folder-id name)))
      (cocoa.folder:save-folders folder-repos
       (list (cocoa.folder:make-folder-config
              :id id
              :name name
              :thumbnail thumbnail
              :modified-at (get-universal-time))))
      (funcall (get-by-id folder-repos) id))))


@export
(defun exec (prepare &key name thumbnail)
  (funcall prepare :name name :thumbnail thumbnail))
