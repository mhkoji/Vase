(in-package :vase.app.folder)

(defun list-folder-overviews (db from size)
  (->> (vase.entities.folder.repository:load-by-range db from size)
       (mapcar #'folder->resp)))
(export 'list-folder-overviews)

#+nil
(defun search-folder-overviews (db name)
  (->> (vase.entities.folder.repository:search-by-name db name)
       (mapcar #'folder->resp)))
