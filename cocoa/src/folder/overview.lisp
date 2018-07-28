(in-package :cocoa.folder)

(defun list-folder-overviews (db from size)
  (->> (cocoa.entity.folder.repository:load-by-range db from size)
       (mapcar #'folder->resp)))
(export 'list-folder-overviews)

#+nil
(defun search-folder-overviews (db name)
  (->> (cocoa.entity.folder.repository:search-by-name db name)
       (mapcar #'folder->resp)))
