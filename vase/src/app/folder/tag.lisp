(in-package :vase.app.folder)

(defun tag->resp (tag)
  (list :id (vase.entities.tag:tag-id tag)
        :name (vase.entities.tag:tag-name tag)))


(defun as-tagged-content (folder-id)
  (vase.app.folder.util:accept-folder-id folder-id)
  (make-instance 'vase.entities.tag:content :id folder-id :type :folder))

(defclass folder-container ()
  ((db :initarg :db)))
(export 'folder-container)

(defmethod vase.entities.tag:render-contents ((container folder-container)
                                             (type (eql :folder))
                                             (content-ids list))
  (let ((db (slot-value container 'db)))
    (->> (vase.entities.folder.repository:load-by-ids db content-ids)
         (mapcar #'folder->resp))))

(defun set-folder-tags (db folder-id tag-ids)
  (let ((content (as-tagged-content folder-id)))
    (dolist (tag (vase.entities.tag.repository:load-by-content db
                                                              content))
      (vase.entities.tag.repository:detach-tag tag content))
    (dolist (tag (vase.entities.tag.repository:load-by-ids db
                                                          tag-ids))
      (vase.entities.tag.repository:attach-tag tag content))))
(export 'set-folder-tags)

(defun get-folder-tags (db folder-id)
  (let ((content (as-tagged-content folder-id)))
    (->> (vase.entities.tag.repository:load-by-content db content)
         (mapcar #'tag->resp))))
(export 'get-folder-tags)
