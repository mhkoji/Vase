(in-package :vase.tag)

(defun content->content-row (content)
  (vase.tag.db:make-content-row
   :id (content-id content)
   :type (symbol-name (content-type content))))

(defun tag->row (tag)
  (vase.tag.db:make-row
   :tag-id (tag-id tag)
   :name (tag-name tag)))

(defun row->tag (r)
  (make-instance 'tag
   :id (vase.tag.db:row-tag-id r)
   :name (vase.tag.db:row-name r)))


(defun update (db tag)
  (vase.tag.db:update db (tag->row tag)))

(defun save (db name)
  (row->tag (vase.tag.db:insert db name)))


(defun attach-tag (db tag content)
  (vase.tag.db:content/insert
   db (content->content-row content) (list (tag-id tag)))
  (values))

(defun detach-tag (db tag content)
  (vase.tag.db:content/delete
   db (content->content-row content) (list (tag-id tag)))
  (values))


(defun bulk-load-by-ids (db ids)
  (mapcar #'row->tag (vase.tag.db:select-by-ids db ids)))

(defun bulk-load-by-range (db offset size)
  (mapcar #'row->tag (vase.tag.db:select-by-range db offset size)))

(defun bulk-load-by-content (db content)
  (mapcar #'row->tag (vase.tag.db:content/select-tags
                      db
                      (content->content-row content))))

(defun bulk-load-contents-by-tag (content-repos db tag)
  (let ((local-ids nil)
        (type->local-ids (make-hash-table))
        (local-id->content-id (make-hash-table :test #'equal))
        (local-id->content (make-hash-table :test #'equal)))
    (loop for local-id from 0
          for content-row in (vase.tag.db:content/select db (tag-id tag))
          do (let ((type (alexandria:make-keyword
                          (vase.tag.db:content-row-type content-row)))
                   (content-id (vase.tag.db:content-row-id content-row)))
               (push local-id local-ids)
               (push local-id (gethash type type->local-ids))
               (setf (gethash local-id local-id->content-id) content-id)))
    (loop for type being the hash-keys of type->local-ids
          for local-ids = (gethash type type->local-ids)
          for content-ids = (mapcar (lambda (local-id)
                                      (gethash local-id
                                               local-id->content-id))
                                    local-ids)
          for contents = (vase.tag.contents:bulk-load content-repos
                                                      type
                                                      content-ids)
          do (loop for local-id in local-ids
                   for content in contents
                   do (setf (gethash local-id local-id->content) content)))
    (mapcar (lambda (local-id) (gethash local-id local-id->content))
            local-ids)))



(defun tag-contents (tag db content-repos)
  (bulk-load-contents-by-tag content-repos db tag))


(defun bulk-delete (content-repos db ids)
  ;; Delete the contents attached with tags
  (dolist (tag (bulk-load-by-ids db ids))
    (dolist (content (tag-contents tag db content-repos))
      (detach-tag db tag content)))
  (vase.tag.db:delete db ids)
  (values))


(defun content-tags (content db)
  (bulk-load-by-content db content))


(defun set-content-tags (db content tags)
  (dolist (tag (bulk-load-by-content db content))
    (detach-tag db tag content))
  (dolist (tag tags)
    (attach-tag db tag content))
  (values))
