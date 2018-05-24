(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

(defun thumbnail->dto (thumbnail)
  (list :id (cocoa.use-case.folder.thumbnail:thumbnail->image-id
             thumbnail)))

(defun folder->dto (folder)
  (list :id (folder-id folder)
        :name (folder-name folder)
        :thumbnail (thumbnail->dto (folder-thumbnail folder))))

(defmacro with-output (&body body)
  `(mapcar #'folder->dto (progn ,@body)))

@export
(defun list/range (from size &key folder-repository)
  (mapcar #'folder->dto
          (list-folders/range folder-repository
                              (make-list-spec :with-thumbnail-p t)
                              from size)))

@export
(defun list/ids (ids &key folder-repository)
  (mapcar #'folder->dto
          (list-folders/ids folder-repository
                            (make-list-spec :with-thumbnail-p t)
                            ids)))

@export
(defun get/id (id &key folder-repository)
  (folder->dto (car (list-folders/ids folder-repository
                                      (make-list-spec)
                                      (list id)))))

@export
(defun search/name (name &key folder-repository)
  (with-output
    (search-folders/name folder-repository
                         (make-list-spec :with-thumbnail-p t)
                         name)))
