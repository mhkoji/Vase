(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

@export
(defun list-folder-overviews (from size &key db)
  (->> (cocoa.entity.folder.repository:load-by-range db from size)
       (mapcar #'folder->resp)))

@export
(defun search-folder-overviews (name &key db)
  (->> (cocoa.entity.folder.repository:search-by-name db name)
       (mapcar #'folder->resp)))
