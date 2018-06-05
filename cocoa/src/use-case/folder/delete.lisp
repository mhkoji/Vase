(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

@export
(defun delete/id (folder-id &key folder-dao)
  (cocoa.entity.folder:delete-by-ids! folder-dao (list folder-id)))
