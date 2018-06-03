(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

@export
(defgeneric handle! (dao action))

@export
(defmacro do-update!* (dao &body body)
  "The macro for adding new folders to the current system."
  (let ((gdao (gensym))
        (actions (gensym)))
    `(let ((,gdao ,dao)
           (,actions nil))
       (labels ((act! (action)
                  (push action ,actions)))
         (progn ,@body))
       (dolist (action (nreverse ,actions) ,gdao)
         (setq ,gdao (handle! ,gdao action))))))
(export 'act!)

@export
(defmacro do-update! (dao &body body)
  `(do-update!* ,dao
     (act! ,@body)))


(defclass add-action ()
  ((config :initarg :config)))

(defmethod handle! (dao (action add-action))
  (save! dao (list (slot-value action 'config))))

@export
(defun add (&key id name thumbnail modified-at)
  (make-instance 'add-action
   :config (make-folder-config :id id
                               :name name
                               :thumbnail thumbnail
                               :modified-at modified-at)))


(defclass bulk-add-action ()
  ((configs :initarg :configs)))

(defmethod handle! (dao (action bulk-add-action))
  (save! dao (slot-value action 'configs)))

@export
(defun bulk-add (add-actions)
  (let ((configs (mapcar (lambda (action) (slot-value action 'config))
                         add-actions)))
    (make-instance 'bulk-add-action :configs configs)))


(defclass change-thumbnail-action ()
  ((folder-id :initarg :folder-id)
   (thumbanil-id :initarg :thumbnail-id)))

(defmethod handle! (dao (action change-thumbnail-action))
  (let ((folder-id (slot-value action 'folder-id))
        (thumbnail-id (slot-value action 'thumbnail-id)))
    (-> dao
        (folder-thumbnail-delete
         (list folder-id))
        (folder-thumbnail-insert
         (list (make-thumbnail-row :folder-id folder-id
                                   :thumbnail-id thumbnail-id))))))

@export
(defun change-thumbnail (folder-id thumbanil-id)
  ;; Any verification needed for this action should be writte here
  (make-instance 'change-thumbnail-action
                 :folder-id folder-id :thumbnail-id thumbanil-id))



(defclass append-contents-action ()
  ((folder-id :initarg :folder-id)
   (contents :initarg :contents)))

(defmethod handle! (dao (action append-contents-action))
  (let ((folder-id (slot-value action 'folder-id))
        (content-ids (mapcar #'content-id (slot-value action 'contents))))
    (folder-content-insert dao folder-id content-ids)))

@export
(defun append-contents (folder-id contents)
  ;; Any verification needed for this action should be writte here
  (make-instance 'append-contents-action
                 :folder-id folder-id :contents contents))
