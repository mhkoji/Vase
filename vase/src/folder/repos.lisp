(in-package :vase.folder)

(defun folder->folder-row (folder)
  (vase.folder.db.folder:make-row
   :folder-id (folder-id folder)
   :name (folder-name folder)
   :modified-at (folder-modified-at folder)))

(defun folder->thumbnail-row (folder)
  (vase.folder.db.thumbnail:make-row
   :folder-id (folder-id folder)
   :thumbnail-id (vase.folder.thumbnail:thumbnail-id (folder-thumbnail
                                                      folder))))


(defun bulk-save (db folders)
  (let ((folder-rows (mapcar #'folder->folder-row folders))
        (thumbnail-rows (mapcar #'folder->thumbnail-row folders)))
    (vase.folder.db.folder:insert db folder-rows)
    (vase.folder.db.thumbnail:insert db thumbnail-rows)))


(defun bulk-delete (db ids)
  (vase.folder.db.thumbnail:delete db ids)
  (vase.folder.db.folder:delete db ids))


(defstruct repository db thumbnail-repos)

(defun bulk-load-by-ids (repos folder-ids)
  "Returns the folders with the given ids"
  (let ((db (repository-db repos))
        (thumbnail-repos (repository-thumbnail-repos repos))
        (folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))

    (dolist (row (vase.folder.db.folder:select db folder-ids))
      (let ((folder-id (vase.folder.db.folder:row-folder-id row)))
        (setf (gethash folder-id folder-id->row) row)))

    (let ((rows (vase.folder.db.thumbnail:select db folder-ids)))
      (let ((thumbnail-id->thumbnail (make-hash-table :test #'equal))
            (thumbnails
             (vase.folder.thumbnail:bulk-load
              thumbnail-repos
              (mapcar #'vase.folder.db.thumbnail:row-thumbnail-id
                      rows))))
        (dolist (thumbnail thumbnails)
          (setf (gethash (vase.folder.thumbnail:thumbnail-id thumbnail)
                         thumbnail-id->thumbnail)
                thumbnail))
        (dolist (row rows)
          (let ((folder-id (vase.folder.db.thumbnail:row-folder-id row))
                (thumbnail
                 (gethash
                  (vase.folder.db.thumbnail:row-thumbnail-id row)
                  thumbnail-id->thumbnail)))
            (setf (gethash folder-id folder-id->thumbnail) thumbnail)))))

    (loop for id in folder-ids
          for row = (gethash id folder-id->row)
          when row
            collect (make-instance 'folder
                     :id id
                     :name (vase.folder.db.folder:row-name row)
                     :thumbnail (gethash id folder-id->thumbnail)
                     :modified-at
                     (vase.folder.db.folder:row-modified-at row)))))


(defun bulk-load-by-range (repos offset size)
  (let ((ids (vase.folder.db.folder:select-ids
              (repository-db repos)
              offset
              size)))
    (bulk-load-by-ids repos ids)))


(defun bulk-load-by-search (repos name)
  (let ((ids (vase.folder.db.folder:search-ids
              (repository-db repos)
              name)))
    (bulk-load-by-ids repos ids)))

(defun load-by-id (repos id)
  (car (bulk-load-by-ids repos (list id))))
