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

@export
(defun list-by-range (from size &key folder-dao)
  (->> (cocoa.entity.folder:list-by-range folder-dao from size)
       (mapcar #'folder->dto)))

@export
(defun list-by-ids (ids &key folder-dao)
  (->> (cocoa.entity.folder:list-by-ids folder-dao ids)
       (mapcar #'folder->dto)))

@export
(defun get-by-id (id &key folder-dao)
  (->> (car (cocoa.entity.folder:list-by-ids folder-dao (list id)))
       folder->dto))

@export
(defun search-by-name (name &key folder-dao)
  (->> (cocoa.entity.folder:search-by-name folder-dao name)
       (mapcar #'folder->dto)))
