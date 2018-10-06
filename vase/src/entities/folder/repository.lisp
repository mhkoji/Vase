(defpackage :vase.entities.folder.repository
  (:use :cl
        :vase.entities.folder
        :vase.entities.folder.db)
  (:import-from :cl-arrows :->))
(in-package :vase.entities.folder.repository)

(defun load-by-ids (db ids)
  "Returns the folders with the given ids"
  (let ((folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))

    (dolist (row (folder-select db ids))
      (setf (gethash (folder-row-folder-id row) folder-id->row) row))

    (dolist (row (folder-thumbnail-select db ids))
      (let ((folder-id (thumbnail-row-folder-id row))
            (thumbnail (make-thumbnail (thumbnail-row-thumbnail-id row))))
        (setf (gethash folder-id folder-id->thumbnail) thumbnail)))

    (loop for id in ids
          for row = (gethash id folder-id->row)
      when row collect (make-folder
                        :id id
                        :name (folder-row-name row)
                        :thumbnail (gethash id folder-id->thumbnail)
                        :modified-at (folder-row-modified-at row)))))
(export 'load-by-ids)

(defun load-by-range (db offset size)
  "Returns the folders within the range"
  (load-by-ids db (folder-select-ids db offset size)))
(export 'load-by-range)

#+nil
(defun search-by-name (db keyword)
  "Returns the folders whose names contain the keyword"
  (load-by-ids db (folder-search-ids db keyword)))

(defun delete-by-ids (db ids)
  "Delete the folders"
  (-> db
      (folder-thumbnail-delete ids)
      (folder-delete ids)))
(export 'delete-by-ids)

(defun folder-thumbnail-row (db folder)
  (make-thumbnail-row db
                      (folder-id folder)
                      (thumbnail-id (folder-thumbnail folder))))

(defun folder-folder-row (db folder)
  (make-folder-row db
                   (folder-id folder)
                   (folder-name folder)
                   (folder-modified-at folder)))

(defun save-bulk (db folders)
  (let ((folder-rows
         (mapcar (alexandria:curry #'folder-folder-row db)
                 folders))
        (thumbnail-rows
         (mapcar (alexandria:curry #'folder-thumbnail-row db)
                 folders)))
    (-> db
        ;; Delete the existing folders
        (delete-by-ids (mapcar #'folder-id folders))
        ;; Insert folders
        (folder-insert folder-rows)
        ;; Insert thumbnails
        (folder-thumbnail-insert thumbnail-rows))))
(export 'save-bulk)

#+nil
(defun update (db folder)
  (-> db
      (folder-thumbnail-delete (list (folder-id folder)))
      (folder-thumbnail-insert (list folder))))
