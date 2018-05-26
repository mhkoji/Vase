(defpackage :cocoa.infra.db.folder.dao
  (:use :cl :cocoa.entity.folder))
(in-package :cocoa.infra.db.folder.dao)
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
(defclass dao () ())

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


(defun source->folder-row (source)
  (make-folder-row :id (source-folder-id source)
                   :name (source-name source)
                   :modified-at (source-modified-at source)))

(defun source->thumbnail-row (source)
  (make-thumbnail-row
   :folder-id (source-folder-id source)
   :thumbnail-id (thumbnail-id (source-thumbnail source))))


(defmethod add-folders/sources ((dao dao) (sources list))
  ;; Delete existing folders
  (setq dao (delete-folders/ids
             dao (mapcar #'source-folder-id sources)))
  ;; Create folders
  (setq dao (folder-insert
             dao (mapcar #'source->folder-row sources)))
  ;; Thumbnail
  (setq dao (folder-thumbnail-insert
             dao (mapcar #'source->thumbnail-row sources)))
  dao)


(defclass simple-thumbnail ()
  ((thumbnail-id
    :initarg :thumbnail-id
    :reader thumbnail-id)))

(defun id->thumbnail (id)
  (make-instance 'simple-thumbnail :thumbnail-id id))

(defclass dao-folder ()
  ((id
    :initarg :id
    :reader folder-id)
   (name
    :initarg :name
    :reader folder-name)
   (thumbnail
    :initarg :thumbnail
    :reader %folder-thumbnail)
   (dao :initarg :dao :reader dao-folder-dao)))

(defmethod list-folders/ids ((dao dao) (spec list-spec) (ids list))
  (let ((folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))
    (dolist (row (folder-select dao ids))
      (setf (gethash (folder-row-id row) folder-id->row) row))
    (when (list-spec-with-thumbnail-p spec)
      (dolist (row (folder-thumbnail-select dao ids))
        (let ((folder-id (thumbnail-row-folder-id row))
              (thumbnail (id->thumbnail (thumbnail-row-thumbnail-id row))))
          (setf (gethash folder-id folder-id->thumbnail) thumbnail))))
    (mapcar (lambda (id)
              (let ((row (gethash id folder-id->row))
                    (thumbnail (gethash id folder-id->thumbnail)))
                (make-instance 'dao-folder
                               :id id
                               :name (folder-row-name row)
                               :thumbnail thumbnail
                               :dao dao)))
            ids)))

(defmethod list-folders/range ((dao dao) (spec list-spec) offset size)
  (list-folders/ids dao spec (folder-select-ids dao offset size)))

(defmethod folder-thumbnail ((folder dao-folder))
  (or (%folder-thumbnail folder)
      (with-accessors ((dao dao-folder-dao)
                       (folder-id folder-id)) folder
        (id->thumbnail
         (thumbnail-row-thumbnail-id
          (car (folder-thumbnail-select dao (list folder-id))))))))

(defmethod delete-folders/ids ((dao dao) (ids list))
  (folder-thumbnail-delete dao ids)
  (folder-content-delete dao ids)
  (folder-delete dao ids)
  dao)

(defmethod search-folders/name ((dao dao) (spec list-spec) (keyword string))
  (list-folders/ids dao spec (folder-search-ids dao keyword)))
