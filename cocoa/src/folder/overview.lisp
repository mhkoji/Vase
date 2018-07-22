(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

@export
(defun list-folder-overviews (from size &key folder-repository)
  (->> (cocoa.entity.folder:load-folders-by-range folder-repository
                                                  from size)
       (mapcar #'folder->resp)))

@export
(defun search-folder-overviews (name &key folder-repository)
  (->> (cocoa.entity.folder:search-folders-by-name folder-repository
                                                   name)
       (mapcar #'folder->resp)))
