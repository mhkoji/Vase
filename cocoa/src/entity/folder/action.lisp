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

@export
(defun add (&key id name thumbnail modified-at)
  (make-instance 'add-action
                 :config (make-folder-config
                          :id id
                          :name name
                          :thumbnail thumbnail
                          :modified-at modified-at)))

(defmethod handle! (dao (action add-action))
  (save! dao (list (slot-value action 'config))))


(defclass action ()
  ((update-dao :initarg :update-dao)))

(defmethod handle! (dao (action action))
  (funcall (slot-value action 'update-dao) dao))

(defmacro do-update-dao! ((dao) &body body)
  `(make-instance 'action :update-dao (lambda (,dao) ,@body)))


@export
(defun bulk-add (add-actions)
  (labels ((action-config (action)
             (slot-value action 'config)))
    (do-update-dao! (dao)
      (save! dao (mapcar #'action-config add-actions)))))

@export
(defun change-thumbnail (folder-id thumbnail-id)
  ;; Any verification needed for this action should be writte here
  (do-update-dao! (dao)
    (-> dao
        (folder-thumbnail-delete (list folder-id))
        (folder-thumbnail-insert
         (list (make-thumbnail-row :folder-id folder-id
                                   :thumbnail-id thumbnail-id))))))

@export
(defun append-contents (folder-id contents)
  ;; Any verification needed for this action should be writte here
  (do-update-dao! (dao)
    (folder-content-insert dao folder-id (mapcar #'content-id contents))))
