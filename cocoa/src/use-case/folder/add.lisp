(defpackage :cocoa.use-case.folder.add
  (:use :cl)
  (:import-from :cl-arrows :-> :-<>))
(in-package :cocoa.use-case.folder.add)
(cl-annot:enable-annot-syntax)

;;; The representation of the source of a folder
(defstruct source name modified-at thumbnail contents)
(export 'make-source)

(defstruct executor folder-dao name->folder-id)
(export 'make-executor)

@export
(defun add-bulk (executor sources)
  (let ((folder-ids (mapcar (alexandria:compose
                             (executor-name->folder-id executor)
                             #'source-name)
                            sources)))
    (-> (executor-folder-dao executor)
        (cocoa.entity.folder:add-all
         (mapcar (lambda (folder-id source)
                   (cocoa.entity.folder:make-folder-config
                    :id folder-id
                    :name (source-name source)
                    :thumbnail (source-thumbnail source)
                    :modified-at (source-modified-at source)))
                 folder-ids sources))
        (cocoa.entity.folder:update-contents
         (cocoa.entity.folder:make-appending-bulk
          :appendings
          (mapcar (lambda (folder-id source)
                    (cocoa.entity.folder:make-appending
                     :folder-id folder-id
                     :contents (source-contents source)))
                  folder-ids sources))))))

@export
(defun add-from-scratch (executor &key name thumbnail)
  (let ((id (funcall (executor-name->folder-id executor) name)))
    (-<> (executor-folder-dao executor)
         (cocoa.entity.folder:add-all
          (list (cocoa.entity.folder:make-folder-config
                 :id id
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
         (cocoa.use-case.folder.get:get-by-id id :folder-dao <>))))

