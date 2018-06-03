(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

@export
(defun create-from-scratch (folder-dao &key name
                                            name->folder-id
                                            thumbnail)
  (let ((id (funcall name->folder-id name)))
    (-<> (cocoa.entity.folder:do-update! folder-dao
           (cocoa.entity.folder:act!
            (cocoa.entity.folder:add
             :id id
             :name name
             :thumbnail thumbnail
             :modified-at (get-universal-time))))
         (get/id id :folder-dao <>))))


@export
(defun change-thumbnail (folder-dao &key folder-id thumbnail-id)
  (cocoa.entity.folder:do-update! folder-dao
    (cocoa.entity.folder:act!
     (cocoa.entity.folder:change-thumbnail folder-id thumbnail-id))))


@export
(defun append-contents (folder-dao &key folder-id contents)
  (cocoa.entity.folder:do-update! folder-dao
    (cocoa.entity.folder:act!
     (cocoa.entity.folder:append-contents folder-id contents))))
