(defpackage :vase.folder.repos
  (:use :cl)
  (:export :folder
           :folder-id
           :folder-name
           :folder-thumbnail
           :folder-modified-at
           :thumbnail-id
           :repository
           :make-repository
           :bulk-load-by-range
           :bulk-load-by-search
           :bulk-load-by-ids
           :bulk-save
           :load-by-id
           :bulk-delete))
(in-package :vase.folder.repos)

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

(defgeneric thumbnail-id (th))


(defun folder->folder-row (folder)
  (vase.folder.repos.db.folder:make-row
   :folder-id (folder-id folder)
   :name (folder-name folder)
   :modified-at (folder-modified-at folder)))

(defun folder->thumbnail-row (folder)
  (vase.folder.repos.db.thumbnail:make-row
   :folder-id (folder-id folder)
   :thumbnail-id (thumbnail-id (folder-thumbnail folder))))

(defun bulk-save (db folders)
  (let ((folder-rows (mapcar #'folder->folder-row folders))
        (thumbnail-rows (mapcar #'folder->thumbnail-row folders)))
    (vase.folder.repos.db.folder:insert db folder-rows)
    (vase.folder.repos.db.thumbnail:insert db thumbnail-rows)))


(defun bulk-delete (db ids)
  (vase.folder.repos.db.thumbnail:delete db ids)
  (vase.folder.repos.db.folder:delete db ids))


(defstruct repository db thumbnail-repos)

(defun bulk-load-by-ids (repos folder-ids)
  "Returns the folders with the given ids"
  (let ((db (repository-db repos))
        (thumbnail-repos (repository-thumbnail-repos repos))
        (folder-id->row (make-hash-table :test #'equal))
        (folder-id->thumbnail (make-hash-table :test #'equal)))

    (dolist (row (vase.folder.repos.db.folder:select db folder-ids))
      (let ((folder-id (vase.folder.repos.db.folder:row-folder-id row)))
        (setf (gethash folder-id folder-id->row) row)))

    (let ((rows (vase.folder.repos.db.thumbnail:select db folder-ids)))
      (let ((thumbnail-id->thumbnail (make-hash-table :test #'equal))
            (thumbnails
             (vase.folder.thumbnail.repos:bulk-load
              thumbnail-repos
              (mapcar #'vase.folder.repos.db.thumbnail:row-thumbnail-id
                      rows))))
        (dolist (thumbnail thumbnails)
          (setf (gethash (thumbnail-id thumbnail) thumbnail-id->thumbnail)
                thumbnail))
        (dolist (row rows)
          (let ((folder-id (vase.folder.repos.db.thumbnail:row-folder-id row))
                (thumbnail
                 (gethash
                  (vase.folder.repos.db.thumbnail:row-thumbnail-id row)
                  thumbnail-id->thumbnail)))
            (setf (gethash folder-id folder-id->thumbnail) thumbnail)))))

    (loop for id in folder-ids
          for row = (gethash id folder-id->row)
          when row
            collect (make-instance 'folder
                     :id id
                     :name (vase.folder.repos.db.folder:row-name row)
                     :thumbnail (gethash id folder-id->thumbnail)
                     :modified-at
                     (vase.folder.repos.db.folder:row-modified-at row)))))


(defun bulk-load-by-range (repos offset size)
  (let ((ids (vase.folder.repos.db.folder:select-ids (repository-db repos)
                                                     offset
                                                     size)))
    (bulk-load-by-ids repos ids)))


(defun bulk-load-by-search (repos name)
  (let ((ids (vase.folder.repos.db.folder:search-ids (repository-db repos)
                                                     name)))
    (bulk-load-by-ids repos ids)))

(defun load-by-id (repos id)
  (car (bulk-load-by-ids repos (list id))))
