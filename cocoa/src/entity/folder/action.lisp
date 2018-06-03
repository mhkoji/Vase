(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

@export
(defgeneric handle! (dao action))


(defstruct add-action config)

@export
(defun add (&key id name thumbnail modified-at)
  (make-add-action :config (make-folder-config
                            :id id
                            :name name
                            :thumbnail thumbnail
                            :modified-at modified-at)))

(defmethod handle! (dao (action add-action))
  (save! dao (list (add-action-config action))))


(defstruct action update-dao)

(defmethod handle! (dao (action action))
  (funcall (action-update-dao action) dao))

(defmacro do-handle! ((dao) &body body)
  `(make-action :update-dao (lambda (,dao) ,@body)))

@export
(defun change-thumbnail (folder-id thumbnail-id)
  ;; Any verification needed for this action should be writte here
  (let ((row (make-thumbnail-row :folder-id folder-id
                                 :thumbnail-id thumbnail-id)))
    (do-handle! (dao)
      (-> dao
          (folder-thumbnail-delete (list folder-id))
          (folder-thumbnail-insert (list row))))))

@export
(defun append-contents (folder-id contents)
  ;; Any verification needed for this action should be writte here
  (do-handle! (dao)
    (folder-content-insert dao folder-id (mapcar #'content-id contents))))


@export
(defgeneric bulk (key actions))

(defmethod bulk ((key (eql :add)) add-actions)
  (do-handle! (dao)
    (save! dao (mapcar #'add-action-config add-actions))))

(defmethod bulk ((key (eql :append-contents)) append-contents-actions)
  (do-handle! (dao)
    (dolist (action append-contents-actions)
      (handle! dao action))))
