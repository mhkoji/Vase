(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

(defstruct folder-repository folder-dao)

@export
(defun folder-repository (folder-dao)
  (make-folder-repository :folder-dao folder-dao))

@export
(defgeneric folder-select (folder-dao ids))
@export
(defgeneric folder-row (folder-dao folder-id name modified-at))
@export
(defgeneric folder-row-folder-id (folder-row))
@export
(defgeneric folder-row-name (folder-row))
@export
(defgeneric folder-row-modified-at (folder-row))

@export
(defgeneric folder-select-ids (folder-dao offset size))
@export
(defgeneric folder-search-ids (folder-dao keyword))
@export
(defgeneric folder-insert (folder-dao folder-rows))
@export
(defgeneric folder-delete (folder-dao folder-id-list))

@export
(defgeneric folder-content-insert (folder-dao folder-id content-id-list))
@export
(defgeneric folder-content-select-ids (folder-dao folder-id))
@export
(defgeneric folder-content-delete (folder-dao folder-id-list))

@export
(defgeneric folder-thumbnail-select (folder-dao folder-id-list))
@export
(defgeneric thumbnail-row-folder-id (thumbnail-row))
@export
(defgeneric thumbnail-row-thumbnail-id (thumbnail-row))
@export
(defgeneric thumbnail-row (folder-dao folder-id thumbnail-id))
@export
(defgeneric folder-thumbnail-insert (folder-dao thumbnail-row-list))
@export
(defgeneric folder-thumbnail-delete (folder-dao folder-id-list))


@export
(defun load-folders-by-ids (folder-repository ids)
  "Returns the folders with the given ids"
  (let ((folder-dao (folder-repository-folder-dao folder-repository))
        (folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))

    (dolist (row (folder-select folder-dao ids))
      (setf (gethash (folder-row-folder-id row) folder-id->row) row))

    (dolist (row (folder-thumbnail-select folder-dao ids))
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
  (let ((folder-dao (folder-repository-folder-dao folder-repository)))
    (let ((folder-ids (folder-select-ids folder-dao offset size)))
      (load-folders-by-ids folder-repository folder-ids))))

@export
(defun search-folders-by-name (folder-repository keyword)
  "Returns the folders whose names contain the keyword"
  (let ((folder-dao (folder-repository-folder-dao folder-repository)))
    (let ((folder-ids (folder-search-ids folder-dao keyword)))
      (load-folders-by-ids folder-repository folder-ids))))

@export
(defun delete-folders-by-ids (folder-repository ids)
  "Delete the folders"
  (make-folder-repository
   :folder-dao
   (-> (folder-repository-folder-dao folder-repository)
       (folder-thumbnail-delete ids)
       (folder-content-delete ids)
       (folder-delete ids))))

(defun folder-thumbnail-row (folder-dao folder)
  (thumbnail-row folder-dao
                 (folder-id folder)
                 (thumbnail-id (folder-thumbnail folder))))

(defun folder-folder-row (folder-dao folder)
  (folder-row folder-dao
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
  (make-folder-repository
   :folder-dao
   (let ((folder-dao (folder-repository-folder-dao folder-repository)))
     (let ((folder-rows
            (mapcar (alexandria:curry #'folder-folder-row
                                      folder-dao)
                    folders))
           (thumbnail-rows
            (mapcar (alexandria:curry #'folder-thumbnail-row
                                      folder-dao)
                    folders)))
       (-> folder-dao
           ;; Insert folders
           (folder-insert folder-rows)
           ;; Insert thumbnails
           (folder-thumbnail-insert thumbnail-rows))))))

@export
(defun update-folder (folder-repository folder)
  (make-folder-repository
   :folder-dao
   (-> (folder-repository-folder-dao folder-repository)
       (folder-thumbnail-delete (list (folder-id folder)))
       (folder-thumbnail-insert (list folder)))))
