(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

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
(defun set-folder-tags (folder-id tag-ids &key tag-repository)
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
(defun get-folder-tags (folder-id &key tag-repository)
  (->> (cocoa.entity.tag:load-tags-by-content
        tag-repository
        (as-tagged-content folder-id))
       (mapcar #'tag->resp)))
