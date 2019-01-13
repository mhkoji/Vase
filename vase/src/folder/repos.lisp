(in-package :vase.folder)

(defun folder->folder-row (folder)
  (vase.db.folder:make-row
   :folder-id (folder-id folder)
   :name (folder-name folder)
   :modified-at (folder-modified-at folder)))

(defun folder->thumbnail-row (folder)
  (vase.db.folder-thumbnail:make-row
   :folder-id (folder-id folder)
   :thumbnail-id (thumbnail-id (folder-thumbnail folder))))


(defun bulk-save (db folders)
  (let ((folder-rows (mapcar #'folder->folder-row folders))
        (thumbnail-rows (mapcar #'folder->thumbnail-row folders)))
    (vase.db.folder:insert db folder-rows)
    (vase.db.folder-thumbnail:insert db thumbnail-rows)))


(defun bulk-delete (db ids)
  (vase.db.folder-thumbnail:delete db ids)
  (vase.db.folder:delete db ids))


(defstruct repository db thumbnail-repos)

(defun bulk-load-by-ids (repos folder-ids)
  "Returns the folders with the given ids"
  (let ((db (repository-db repos))
        (thumbnail-repos (repository-thumbnail-repos repos))
        (folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))

    (dolist (row (vase.db.folder:select db folder-ids))
      (let ((folder-id (vase.db.folder:row-folder-id row)))
        (setf (gethash folder-id folder-id->row) row)))

    (let ((rows (vase.db.folder-thumbnail:select db folder-ids)))
      (let ((thumbnail-id->thumbnail (make-hash-table :test #'equal))
            (thumbnails
             (vase.folder.thumbnail:bulk-load
              thumbnail-repos
              (mapcar #'vase.db.folder-thumbnail:row-thumbnail-id
                      rows))))
        (dolist (thumbnail thumbnails)
          (setf (gethash (vase.folder.thumbnail:thumbnail-id thumbnail)
                         thumbnail-id->thumbnail)
                thumbnail))
        (dolist (row rows)
          (let ((folder-id (vase.db.folder-thumbnail:row-folder-id row))
                (thumbnail
                 (gethash
                  (vase.db.folder-thumbnail:row-thumbnail-id row)
                  thumbnail-id->thumbnail)))
            (setf (gethash folder-id folder-id->thumbnail) thumbnail)))))

    (loop for id in folder-ids
          for row = (gethash id folder-id->row)
          when row
            collect (make-instance 'folder
                     :id id
                     :name (vase.db.folder:row-name row)
                     :thumbnail (gethash id folder-id->thumbnail)
                     :modified-at (vase.db.folder:row-modified-at row)))))


(defun bulk-load-by-range (repos offset size)
  (let ((ids (vase.db.folder:select-ids (repository-db repos) offset size)))
    (bulk-load-by-ids repos ids)))


(defun bulk-load-by-search (repos name)
  (let ((ids (vase.db.folder:search-ids (repository-db repos) name)))
    (bulk-load-by-ids repos ids)))

(defun load-by-id (repos id)
  (car (bulk-load-by-ids repos (list id))))
