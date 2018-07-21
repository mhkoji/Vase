(defpackage :cocoa.folder.overview
  (:use :cl)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.folder.overview)
(cl-annot:enable-annot-syntax)

(defun thumbnail->resp (thumbnail)
  (list :id (cocoa.entity.folder:thumbnail->image-id thumbnail)))

(defun folder->resp (folder)
  (list :id (cocoa.entity.folder:folder-id folder)
        :name (cocoa.entity.folder:folder-name folder)
        :thumbnail (thumbnail->resp (cocoa.entity.folder:folder-thumbnail
                                     folder))))

@export
(defun list-overviews (from size &key folder-repository)
  (->> (cocoa.entity.folder:load-folders-by-range folder-repository
                                                  from size)
       (mapcar #'folder->resp)))

@export
(defun search-overviews (name &key folder-repository)
  (->> (cocoa.entity.folder:search-folders-by-name folder-repository
                                                   name)
       (mapcar #'folder->resp)))
