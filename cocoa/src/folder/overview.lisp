(in-package :cocoa.folder)

(defun list-folder-overviews (from size &key db)
  (->> (cocoa.entity.folder.repository:load-by-range db from size)
       (mapcar #'folder->resp)))
(export 'list-folder-overviews)

#+nil
(defun search-folder-overviews (name &key db)
  (->> (cocoa.entity.folder.repository:search-by-name db name)
       (mapcar #'folder->resp)))
