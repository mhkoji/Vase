(defpackage :cocoa.use-case.folder.add
  (:use :cl)
  (:import-from :cl-arrows :-<>))
(in-package :cocoa.use-case.folder.add)
(cl-annot:enable-annot-syntax)

@export
(defun add (folder-dao name->folder-id &key name thumbnail)
  (let ((id (funcall name->folder-id name)))
    (-<> folder-dao
         (cocoa.entity.folder:add-all
          (list (cocoa.entity.folder:make-folder-config
                 :id id
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
         (cocoa.use-case.folder.list:get-by-id id :folder-dao <>))))
