(defpackage :cocoa.entity.folder.dao
  (:use :cl :cocoa.entity.folder))
(in-package :cocoa.entity.folder.dao)
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

(defmethod save-folders/sources ((dao dao) (sources list))
  ;; Delete existing folders
  (delete-folders/ids dao (mapcar #'source-folder-id sources))
  ;; Create folders
  (folder-insert dao (mapcar #'source->folder-row sources))
  ;; Contents
  (dolist (source sources)
    (let ((folder-id (source-folder-id source))
          (content-id-list (mapcar #'content-id
                                   (source-contents source))))
      (folder-content-insert dao folder-id content-id-list)))
  ;; Thumbnail
  (let ((thumbnail-rows (mapcar #'source->thumbnail-row sources)))
    (folder-thumbnail-insert dao thumbnail-rows))
  dao)

(defclass dao-folder ()
  ((row :initarg :row :reader dao-folder-row)
   (dao :initarg :dao :reader dao-folder-dao)))

(defmethod list-folders/ids ((dao dao) (ids list))
  (let ((id->row (make-hash-table :test #'equal)))
    (dolist (row (folder-select dao ids))
      (setf (gethash (folder-row-id row) id->row) row))
    (mapcar (lambda (id)
              (let ((row (gethash id id->row)))
                (make-instance 'dao-folder :row row :dao dao)))
            ids)))

(defmethod list-folders/range ((dao dao) offset size)
  (list-folders/ids dao (folder-select-ids dao offset size)))

(defmethod folder-id ((folder dao-folder))
  (folder-row-id (dao-folder-row folder)))

(defmethod folder-name ((folder dao-folder))
  (folder-row-name (dao-folder-row folder)))

(defmethod folder-thumbnail ((folder dao-folder))
  (with-accessors ((dao dao-folder-dao)) folder
    (make-simple-thumbnail
     (thumbnail-row-thumbnail-id
      (car (folder-thumbnail-select
            dao
            (list (folder-id folder))))))))

(defmethod folder-contents ((folder dao-folder))
  (with-accessors ((dao dao-folder-dao)) folder
    (mapcar #'make-simple-content
            (folder-content-select-ids dao (folder-id folder)))))

(defmethod delete-folders/ids ((dao dao) (ids list))
  (folder-thumbnail-delete dao ids)
  (folder-content-delete dao ids)
  (folder-delete dao ids)
  dao)

(defmethod query-folder-thumbnails ((dao dao) (folders list))
  (let ((thumbnail-rows
         (folder-thumbnail-select dao (mapcar #'folder-id folders))))
    (let ((folder-ids (mapcar #'thumbnail-row-folder-id
                              thumbnail-rows))
          (thumbnails (mapcar (alexandria:compose
                               #'make-simple-thumbnail
                               #'thumbnail-row-thumbnail-id)
                              thumbnail-rows)))
      (alexandria:alist-hash-table (mapcar #'cons
                                           folder-ids
                                           thumbnails)
                                   :test #'equal))))

(defmethod get-folder-thumbnail (query-ressult folder)
  (gethash (folder-id folder) query-ressult))
