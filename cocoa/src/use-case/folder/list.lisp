(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

(defun thumbnail->dto (thumbnail)
  (list :id (cocoa.use-case.folder.thumbnail:thumbnail->image-id
             thumbnail)))

(defun folder->dto (folder)
  (list :id (cocoa.entity.folder:folder-id folder)
        :name (cocoa.entity.folder:folder-name folder)
        :thumbnail (thumbnail->dto
                    (cocoa.entity.folder:folder-thumbnail folder))))

(defparameter *bulk-fetching-spec*
  (cocoa.entity.folder:make-list-spec :with-thumbnail-p t))

@export
(defun list/range (from size &key folder-dao)
  (mapcar #'folder->dto (cocoa.entity.folder:list-by-range
                         folder-dao *bulk-fetching-spec* from size)))

@export
(defun list/ids (ids &key folder-dao)
  (mapcar #'folder->dto (cocoa.entity.folder:list-by-ids
                         folder-dao *bulk-fetching-spec* ids)))

@export
(defun get/id (id &key folder-dao)
  (folder->dto (car (cocoa.entity.folder:list-by-ids
                     folder-dao
                     (cocoa.entity.folder:make-list-spec)
                     (list id)))))

@export
(defun search/name (name &key folder-dao)
  (mapcar #'folder->dto (cocoa.entity.folder:search-by-name
                         folder-dao *bulk-fetching-spec* name)))
