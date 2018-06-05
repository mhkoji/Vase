(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

@export
(defun create-from-scratch (folder-dao &key name
                                            name->folder-id
                                            thumbnail)
  (let ((id (funcall name->folder-id name)))
    (-<> (cocoa.entity.folder:save-all! folder-dao
          (list (cocoa.entity.folder:make-folder-config
                 :id id
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
         (get/id id :folder-dao <>))))


@export
(defun change-thumbnail (folder-dao &key folder-id image-id)
  (let ((folder (car (cocoa.entity.folder:list-by-ids
                      folder-dao
                      (list folder-id)))))
    (setf (folder-thumbnail folder)
          (cocoa.use-case.folder.thumbnail:make-of-image image-id))
    #+nil
    (save! folder-dao folder)))


@export
(defun append-contents (folder-dao &key folder-id contents)
  (cocoa.entity.folder:update-contents! folder-dao
   (cocoa.entity.folder:append-contents-op folder-id contents)))
