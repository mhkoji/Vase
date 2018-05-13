(defpackage :cocoa.use-case.folder.list
  (:use :cl
        :cocoa.entity.folder
        :cocoa.use-case.folder.inject))
(in-package :cocoa.use-case.folder.list)
(cl-annot:enable-annot-syntax)

(defun thumbnail->dto (thumbnail)
  (list :id (thumbnail->image-id thumbnail)))

(defun folder->dto (folder &key thumbnail)
  (list :id (folder-id folder)
        :name (folder-name folder)
        :thumbnail
        (thumbnail->dto (or thumbnail
                            (folder-thumbnail folder)))))

(defun folders->dtos (folders query-ressult)
  (loop for folder in folders
        for thumbnail = (get-folder-thumbnail query-ressult folder)
        collect (folder->dto folder :thumbnail thumbnail)))

@export
(defun list/range (from size &key folder-repository)
  (let ((folders (list-folders/range folder-repository from size)))
    (folders->dtos folders
                   (query-folder-thumbnails folder-repository folders))))

@export
(defun list/ids (ids &key folder-repository)
  (let ((folders (list-folders/ids folder-repository ids)))
    (folders->dtos folders
                   (query-folder-thumbnails folder-repository folders))))

@export
(defun get/id (id &key folder-repository)
  (folder->dto (car (list-folders/ids folder-repository (list id)))))
