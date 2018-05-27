;;; The representation of Folder
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

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
(defun delete-by-ids (dao ids)
  "Delete the folders"
  (folder-thumbnail-delete dao ids)
  (folder-content-delete dao ids)
  (folder-delete dao ids)
  dao)


;; What a folder is made from
(defstruct source folder-id name thumbnail modified-at)
(export 'make-source)
(export 'source-folder-id)
(export 'source-name)
(export 'source-thumbnail)
(export 'source-modified-at)

(defun source->folder-row (source)
  (make-folder-row
   :id (source-folder-id source)
   :name (source-name source)
   :modified-at (source-modified-at source)))

(defun source->thumbnail-row (source)
  (make-thumbnail-row
   :folder-id (source-folder-id source)
   :thumbnail-id (thumbnail-id (source-thumbnail source))))

@export
(defun save (dao sources)
  "Save folders"
  ;; Delete the existing folders
  (setq dao (delete-by-ids dao
             (mapcar #'source-folder-id sources)))
  ;; Insert folders
  (setq dao (folder-insert dao
             (mapcar #'source->folder-row sources)))
  ;; Insert thumbnails
  (setq dao (folder-thumbnail-insert dao
             (mapcar #'source->thumbnail-row sources)))
  dao)


(defclass folder ()
  ((dao :initarg :dao)
   (id :initarg :id)
   (name :initarg :name)
   (thumbnail :initarg :thumbnail)))

(defclass folder-thumbnail ()
  ((thumbnail-id
    :initarg :thumbnail-id
    :reader thumbnail-id)))

(defun id->thumbnail (id)
  (make-instance 'folder-thumbnail :thumbnail-id id))


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
  (or (slot-value folder 'thumbnail)
      (with-slots ((dao dao) (folder-id id)) folder
        (id->thumbnail
         (thumbnail-row-thumbnail-id
          (car (folder-thumbnail-select dao (list folder-id))))))))

;; The specification of listing folders
(defstruct list-spec with-thumbnail-p)
(export 'make-list-spec)

@export
(defun list-by-ids (dao list-spec ids)
  "Returns the folders with the given ids"
  (let ((folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))
    (dolist (row (folder-select dao ids))
      (setf (gethash (folder-row-id row) folder-id->row) row))
    (when (list-spec-with-thumbnail-p list-spec)
      (dolist (row (folder-thumbnail-select dao ids))
        (let ((folder-id (thumbnail-row-folder-id row))
              (thumbnail (id->thumbnail (thumbnail-row-thumbnail-id row))))
          (setf (gethash folder-id folder-id->thumbnail) thumbnail))))
    (mapcar (lambda (id)
              (let ((row (gethash id folder-id->row))
                    (thumbnail (gethash id folder-id->thumbnail)))
                (make-instance 'folder
                               :dao dao
                               :id id
                               :name (folder-row-name row)
                               :thumbnail thumbnail)))
            ids)))

@export
(defun list-by-range (dao spec offset size)
  "Returns the folders within the range"
  (list-by-ids dao spec (folder-select-ids dao offset size)))

@export
(defun search-by-name (dao spec keyword)
  "Returns the folders whose names contain the keyword"
  (list-by-ids dao spec (folder-search-ids dao keyword)))
