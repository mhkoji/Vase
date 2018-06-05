;;; The representation of Folder
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

(defclass folder ()
  ((id :initarg :id)
   (name :initarg :name)
   (thumbnail :initarg :thumbnail)))

@export
(defun folder-id (folder)
  "Returns the unique id of a content"
  (slot-value folder 'id))

@export
(defun folder-name (folder)
  "Returns the name of a folder"
  (slot-value folder 'name))

@export
(defun folder-thumbnail (folder)
  "Returns the thumbanil of a folder"
  (slot-value folder 'thumbnail))

(defun (setf folder-thumbnail) (thumbnail folder)
  (setf (slot-value folder 'thumbnail) thumbnail))


;; A folder configuration from which a folder is saved
(defstruct folder-config id name thumbnail modified-at)
(export 'make-folder-config)


(defclass folder-thumbnail ()
  ((thumbnail-id
    :initarg :thumbnail-id
    :reader thumbnail-id)))

(defun id->thumbnail (id)
  (make-instance 'folder-thumbnail :thumbnail-id id))


(defstruct folder-row id name modified-at)
(export 'make-folder-row)
(export 'folder-row-id)
(export 'folder-row-name)
(export 'folder-row-modified-at)

(defstruct thumbnail-row folder-id thumbnail-id)
(export 'make-thumbnail-row)
(export 'thumbnail-row-folder-id)
(export 'thumbnail-row-thumbnail-id)

@export
(defgeneric folder-insert (dao folder-row-list))
@export
(defgeneric folder-select (dao ids))
@export
(defgeneric folder-select-ids (dao offset size))
@export
(defgeneric folder-search-ids (dao keyword))
@export
(defgeneric folder-delete (dao folder-id-list))


@export
(defgeneric folder-content-insert (dao folder-id content-id-list))
@export
(defgeneric folder-content-select-ids (dao folder-id))
@export
(defgeneric folder-content-delete (dao folder-id-list))


@export
(defgeneric folder-thumbnail-insert (dao thumbnail-row-list))
@export
(defgeneric folder-thumbnail-select (dao folder-id-list))
@export
(defgeneric folder-thumbnail-delete (dao folder-id-list))


@export
(defun delete-by-ids! (dao ids)
  "Delete the folders"
  (folder-thumbnail-delete dao ids)
  (folder-content-delete dao ids)
  (folder-delete dao ids)
  dao)


(defun folder-config-folder-row (config)
  (make-folder-row
   :id (folder-config-id config)
   :name (folder-config-name config)
   :modified-at (folder-config-modified-at config)))

(defun folder-config-thumbnail-row (config)
  (make-thumbnail-row
   :folder-id (folder-config-id config)
   :thumbnail-id (thumbnail-id (folder-config-thumbnail config))))

@export
(defun save-all! (dao configs)
  (-> dao
      ;; Delete the existing folders
      (delete-by-ids! (mapcar #'folder-config-id configs))
      ;; Insert folders
      (folder-insert (mapcar #'folder-config-folder-row configs))
      ;; Insert thumbnails
      (folder-thumbnail-insert (mapcar #'folder-config-thumbnail-row
                                       configs))))


;; The specification of listing folders
@export
(defun list-by-ids (dao ids)
  "Returns the folders with the given ids"
  (let ((folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))
    (dolist (row (folder-select dao ids))
      (setf (gethash (folder-row-id row) folder-id->row) row))
    (dolist (row (folder-thumbnail-select dao ids))
      (let ((folder-id (thumbnail-row-folder-id row))
            (thumbnail (id->thumbnail (thumbnail-row-thumbnail-id row))))
        (setf (gethash folder-id folder-id->thumbnail) thumbnail)))
    (mapcar (lambda (id)
              (let ((row (gethash id folder-id->row))
                    (thumbnail (gethash id folder-id->thumbnail)))
                (make-instance 'folder
                               :id id
                               :name (folder-row-name row)
                               :thumbnail thumbnail)))
            ids)))

@export
(defun list-by-range (dao offset size)
  "Returns the folders within the range"
  (list-by-ids dao (folder-select-ids dao offset size)))

@export
(defun search-by-name (dao keyword)
  "Returns the folders whose names contain the keyword"
  (list-by-ids dao (folder-search-ids dao keyword)))
