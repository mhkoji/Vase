(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

@export
(defun delete/id (folder-id &key folder-repository)
  (cocoa.entity.folder:delete-folders/ids folder-repository
                                          (list folder-id)))
