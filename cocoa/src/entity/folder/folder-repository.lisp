(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

(defstruct repository dao)

@export
(defun folder-repository (dao)
  (make-repository :dao dao))

@export
(defgeneric folder-select (dao ids))
@export
(defgeneric folder-row (dao folder-id name modified-at))
@export
(defgeneric folder-row-folder-id (folder-row))
@export
(defgeneric folder-row-name (folder-row))
@export
(defgeneric folder-row-modified-at (folder-row))

@export
(defgeneric folder-select-ids (dao offset size))
@export
(defgeneric folder-search-ids (dao keyword))
@export
(defgeneric folder-insert (dao folder-rows))
@export
(defgeneric folder-delete (dao folder-id-list))

@export
(defgeneric folder-content-insert (dao folder-id content-id-list))
@export
(defgeneric folder-content-select-ids (dao folder-id))
@export
(defgeneric folder-content-delete (dao folder-id-list))

@export
(defgeneric folder-thumbnail-select (dao folder-id-list))
@export
(defgeneric thumbnail-row-folder-id (thumbnail-row))
@export
(defgeneric thumbnail-row-thumbnail-id (thumbnail-row))
@export
(defgeneric thumbnail-row (dao folder-id thumbnail-id))
@export
(defgeneric folder-thumbnail-insert (dao thumbnail-row-list))
@export
(defgeneric folder-thumbnail-delete (dao folder-id-list))


@export
(defun load-folders-by-ids (folder-repository ids)
  "Returns the folders with the given ids"
  (let ((dao (repository-dao folder-repository))
        (folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))

    (dolist (row (folder-select dao ids))
      (setf (gethash (folder-row-folder-id row) folder-id->row) row))

    (dolist (row (folder-thumbnail-select dao ids))
      (let ((folder-id (thumbnail-row-folder-id row))
            (thumbnail (make-thumbnail (thumbnail-row-thumbnail-id row))))
        (setf (gethash folder-id folder-id->thumbnail) thumbnail)))

    (mapcar (lambda (id)
              (let ((row (gethash id folder-id->row)))
                (make-folder
                 :id id
                 :name (folder-row-name row)
                 :thumbnail (gethash id folder-id->thumbnail)
                 :modified-at (folder-row-modified-at row))))
            ids)))

@export
(defun load-folders-by-range (folder-repository offset size)
  "Returns the folders within the range"
  (let ((dao (repository-dao folder-repository)))
    (let ((folder-ids (folder-select-ids dao offset size)))
      (load-folders-by-ids folder-repository folder-ids))))

@export
(defun search-folders-by-name (folder-repository keyword)
  "Returns the folders whose names contain the keyword"
  (let ((dao (repository-dao folder-repository)))
    (let ((folder-ids (folder-search-ids dao keyword)))
      (load-folders-by-ids folder-repository folder-ids))))

@export
(defun delete-folders-by-ids (folder-repository ids)
  "Delete the folders"
  (make-repository
   :dao
   (-> (repository-dao folder-repository)
       (folder-thumbnail-delete ids)
       (folder-content-delete ids)
       (folder-delete ids))))

(defun folder-thumbnail-row (dao folder)
  (thumbnail-row dao
                 (folder-id folder)
                 (thumbnail-id (folder-thumbnail folder))))

(defun folder-folder-row (dao folder)
  (folder-row dao
              (folder-id folder)
              (folder-name folder)
              (folder-modified-at folder)))

;; A folder configuration from which the folder is saved
@export
(defun save-folders (folder-repository folders)
  ;; Delete the existing folders
  (setq folder-repository (delete-folders-by-ids
                           folder-repository
                           (mapcar #'folder-id folders)))
  (make-repository
   :dao
   (let ((dao (repository-dao folder-repository)))
     (let ((folder-rows
            (mapcar (alexandria:curry #'folder-folder-row dao)
                    folders))
           (thumbnail-rows
            (mapcar (alexandria:curry #'folder-thumbnail-row dao)
                    folders)))
       (-> dao
           ;; Insert folders
           (folder-insert folder-rows)
           ;; Insert thumbnails
           (folder-thumbnail-insert thumbnail-rows))))))

@export
(defun update-folder (folder-repository folder)
  (make-repository
   :dao
   (-> (repository-dao folder-repository)
       (folder-thumbnail-delete (list (folder-id folder)))
       (folder-thumbnail-insert (list folder)))))
