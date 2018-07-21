(defpackage :cocoa.folder.tag
  (:use :cl)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.folder.tag)
(cl-annot:enable-annot-syntax)

(defun thumbnail->resp (thumbnail)
  (list :id (cocoa.entity.folder:thumbnail->image-id thumbnail)))

(defun folder->resp (folder)
  (list :id (cocoa.entity.folder:folder-id folder)
        :name (cocoa.entity.folder:folder-name folder)
        :thumbnail (thumbnail->resp (cocoa.entity.folder:folder-thumbnail
                                     folder))))
(defun tag->resp (tag)
  (list :id (cocoa.entity.tag:tag-id tag)
        :name (cocoa.entity.tag:tag-name tag)))


(defun as-tagged-content (folder-id)
  (cocoa.folder.util:accept-folder-id folder-id)
  (make-instance 'cocoa.entity.tag:content :id folder-id :type :folder))

@export
(defclass folder-container ()
  ((folder-repository :initarg :folder-repository)))

(defmethod cocoa.entity.tag:render-contents ((container folder-container)
                                             (type (eql :folder))
                                             (content-ids list))
  (let ((folder-repos (slot-value container 'folder-repository)))
    (->> (cocoa.entity.folder:load-folders-by-ids folder-repos content-ids)
         (mapcar #'folder->resp))))

@export
(defun set-tags (folder-id tag-ids &key tag-repository)
  (let ((content (as-tagged-content folder-id)))
    (dolist (tag (cocoa.entity.tag:load-tags-by-content
                  tag-repository
                  content))
      (cocoa.entity.tag:detach-tag tag content))
    (dolist (tag (cocoa.entity.tag:load-tags-by-ids
                  tag-repository
                  tag-ids))
      (cocoa.entity.tag:attach-tag tag content))))

@export
(defun get-tags (folder-id &key tag-repository)
  (->> (cocoa.entity.tag:load-tags-by-content
        tag-repository
        (as-tagged-content folder-id))
       (mapcar #'tag->resp)))
