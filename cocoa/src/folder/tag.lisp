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
  ((db :initarg :db)))

(defmethod cocoa.entity.tag:render-contents ((container folder-container)
                                             (type (eql :folder))
                                             (content-ids list))
  (let ((db (slot-value container 'db)))
    (->> (cocoa.entity.folder.repository:load-by-ids db content-ids)
         (mapcar #'folder->resp))))

@export
(defun set-folder-tags (folder-id tag-ids &key db)
  (let ((content (as-tagged-content folder-id)))
    (dolist (tag (cocoa.entity.tag.repository:load-by-content db
                                                              content))
      (cocoa.entity.tag.repository:detach-tag tag content))
    (dolist (tag (cocoa.entity.tag.repository:load-by-ids db
                                                          tag-ids))
      (cocoa.entity.tag.repository:attach-tag tag content))))

@export
(defun get-folder-tags (folder-id &key db)
  (let ((content (as-tagged-content folder-id)))
    (->> (cocoa.entity.tag.repository:load-by-content db content)
         (mapcar #'tag->resp))))
