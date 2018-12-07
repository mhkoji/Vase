(defpackage :vase.folder.db
  (:use :cl)
  (:export :folder
           :folder-id
           :folder-name
           :folder-thumbnail
           :thumbnail-id
           :bulk-load-thumbnails-by-ids
           :bulk-delete
           :bulk-save
           :bulk-load-by-ids
           :list-ids-by-range
           :list-ids-by-searching)
  (:import-from :alexandria
                :curry)
  (:import-from :cl-arrows
                :->))
(in-package :vase.folder.db)

(defgeneric thumbnail-id (thumbnail))

(defclass folder ()
  ((id
    :initarg :id
    :reader folder-id)
   (name
    :initarg :name
    :reader folder-name)
   (thumbnail
    :initarg :thumbnail
    :reader folder-thumbnail)
   (modified-at
    :initarg :modified-at
    :reader folder-modified-at)))


(defun bulk-delete (db ids)
  (vase.db.folder.thumbnail:delete db ids)
  (vase.db.folder:delete db ids))

(defun folder->folder-row (db folder)
  (vase.db.folder:make-row
   db
   (folder-id folder)
   (folder-name folder)
   (folder-modified-at folder)))

(defun folder->thumbnail-row (db folder)
  (vase.db.folder.thumbnail:make-row
   db
   (folder-id folder)
   (thumbnail-id (folder-thumbnail folder))))

(defun bulk-save (db folders)
  (let ((folder-rows
         (mapcar (curry #'folder->folder-row db) folders))
        (thumbnail-rows
         (mapcar (curry #'folder->thumbnail-row db) folders)))
    (-> db
        (vase.db.folder:insert folder-rows)
        (vase.db.folder.thumbnail:insert thumbnail-rows))))


(defun bulk-load-by-ids (db thumbnail-repos make-instance folder-ids)
  "Returns the folders with the given ids"
  (let ((folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))

    (dolist (row (vase.db.folder:select db folder-ids))
      (let ((folder-id (vase.db.folder:row-folder-id row)))
        (setf (gethash folder-id folder-id->row) row)))

    (let ((rows (vase.db.folder.thumbnail:select db folder-ids)))
      (let ((thumbnail-id->thumbnail (make-hash-table :test #'equal))
            (thumbnails (vase.folder.thumbnail.repos:bulk-load
                         thumbnail-repos
                         (mapcar #'vase.db.folder.thumbnail:row-thumbnail-id
                                 rows))))
        (dolist (thumbnail thumbnails)
          (setf (gethash (thumbnail-id thumbnail) thumbnail-id->thumbnail)
                thumbnail))
        (dolist (row rows)
          (let ((folder-id (vase.db.folder.thumbnail:row-folder-id row))
                (thumbnail (gethash
                            (vase.db.folder.thumbnail:row-thumbnail-id row)
                            thumbnail-id->thumbnail)))
            (setf (gethash folder-id folder-id->thumbnail) thumbnail)))))

    (loop for id in folder-ids
          for row = (gethash id folder-id->row)
          when row
            collect (funcall make-instance
                     :id id
                     :name (vase.db.folder:row-name row)
                     :thumbnail (gethash id folder-id->thumbnail)
                     :modified-at (vase.db.folder:row-modified-at
                                   row)))))


(defun list-ids-by-range (db offset size)
  (vase.db.folder:select-ids db offset size))


(defun list-ids-by-searching (db name)
  (vase.db.folder:search-ids db name))
