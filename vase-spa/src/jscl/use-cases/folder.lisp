(defpackage :vase.use-cases.folder
  (:use :cl
        :vase.entities.tag
        :vase.entities.tag-edit)
  (:export :show-overviews
           :tag-edit-start
           :tag-edit-submit
           :tag-edit-cancel
           :tag-edit-attach-tag
           :tag-edit-detach-tag))
(in-package :vase.use-cases.folder)

(defun show-overviews (db gui &key offset size)
  (vase.entities.gui.folder:show gui db)
  (vase.api:GET/folders offset size (lambda (folders)
    (format t "~A" folders)
    (vase.entities.folder.repository:save-all db folders))
    (vase.entities.gui.folder:show guid db)))


(defun tag-edit-start (db gui &key folder-id)
  (vase.api:GET/tags (lambda (tags)
    (vase.api.folder:GET/tags folder-id (lambda (attached-tags)
       (let ((edit (make-tag-edit
                    :folder-id folder-id
                    :tags tags
                    :attached-tags attached-tags)))
         (vase.entities.tag-edit.repository:save db tab-edit)
         (vase.entities.gui.folder:show gui db)))))))

(defun tag-edit-cancel (db gui)
  (vase.entities.tag-edit.repository:save db nil)
  (vase.entities.gui.folder:show gui db))

(defmacro with-tag-edit ((edit db) &rest body)
  `(let ((,edit (vase.entities.tag-edit.repository:load ,db)))
     (when (not ,edit)
       (error "no edit"))
     ,@body))

(defun tag-edit-submit (db gui &key sleep-by-sec)
  (with-tag-edit (edit db)
    (let ((folder-id (tag-edit-folder-id edit))
          (attached-tags (tag-edit-attached-tags edit)))
      (vase.api:PUT/folder/tags folder-id attached-tags (lambda ()
        (sleep-by-sec 3 (lambda ()
         (vase.entities.gui.folder:show gui db))))))))

(defun find-tag (tag-edit tag-id)
  (find tag-id (tag-edit-tags edit) :key #'tag-id :test #'string=))

(defun tag-edit-attach-tag (db gui tag-id)
  (with-tag-edit (edit db)
    (let ((tag (find-tag edit tag-id)))
      (when tag
        (pushnew tag (tag-edit-attached-tags edit)
                 :key #'tag-id :test #'string=)
        (vase.entities.tag-edit.repository:save db edit)
        (vase.entities.gui.folder:show gui db)))))

(defun tag-edit-detach-tag (db gui tag-id)
  (with-tag-edit (edit db)
    (let ((tag (find-tag edit tag-id)))
      (when tag
        (delete tag (tag-edit-attached-tags edit)
                :key #'tag-id :test #'string=)
        (vase.entities.tag-edit.repository:save db edit)
        (vase.entities.gui.folder:show gui db)))))
