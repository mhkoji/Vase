(defpackage :cocoa.entity.folder.repository
  (:use :cl :cocoa.entity.folder)
  (:import-from :cl-arrows :->))
(in-package :cocoa.entity.folder.repository)
(cl-annot:enable-annot-syntax)

;;; A primitive language
@export
(defgeneric folder-select (db ids))
@export
(defgeneric folder-row (db folder-id name modified-at))
@export
(defgeneric folder-row-folder-id (folder-row))
@export
(defgeneric folder-row-name (folder-row))
@export
(defgeneric folder-row-modified-at (folder-row))

@export
(defgeneric folder-select-ids (db offset size))
@export
(defgeneric folder-search-ids (db keyword))
@export
(defgeneric folder-insert (db folder-rows))
@export
(defgeneric folder-delete (db folder-id-list))

@export
(defgeneric folder-thumbnail-select (db folder-id-list))
@export
(defgeneric thumbnail-row-folder-id (thumbnail-row))
@export
(defgeneric thumbnail-row-thumbnail-id (thumbnail-row))
@export
(defgeneric thumbnail-row (db folder-id thumbnail-id))
@export
(defgeneric folder-thumbnail-insert (db thumbnail-row-list))
@export
(defgeneric folder-thumbnail-delete (db folder-id-list))


@export
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

    (mapcar (lambda (id)
              (let ((row (gethash id folder-id->row)))
                (make-folder
                 :id id
                 :name (folder-row-name row)
                 :thumbnail (gethash id folder-id->thumbnail)
                 :modified-at (folder-row-modified-at row))))
            ids)))

@export
(defun load-by-range (db offset size)
  "Returns the folders within the range"
  (load-by-ids db (folder-select-ids db offset size)))

@export
(defun search-by-name (db keyword)
  "Returns the folders whose names contain the keyword"
  (load-by-ids db (folder-search-ids db keyword)))

@export
(defun delete-by-ids (db ids)
  "Delete the folders"
  (-> db
      (folder-thumbnail-delete ids)
      (folder-delete ids)))

(defun folder-thumbnail-row (db folder)
  (thumbnail-row db
                 (folder-id folder)
                 (thumbnail-id (folder-thumbnail folder))))

(defun folder-folder-row (db folder)
  (folder-row db
              (folder-id folder)
              (folder-name folder)
              (cocoa.entity.folder::folder-modified-at folder)))

@export
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

@export
(defun update (db folder)
  (-> db
      (folder-thumbnail-delete (list (folder-id folder)))
      (folder-thumbnail-insert (list folder))))
