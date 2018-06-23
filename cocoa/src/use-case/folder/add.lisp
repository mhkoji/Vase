(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

;;; The representation of the source of a folder
(defstruct source name modified-at thumbnail contents)
(export 'make-source)

(defstruct add-folders folder-dao name->folder-id)
(export 'make-add-folders)

@export
(defun bulk-add (add-folders sources)
  (let ((folder-ids (mapcar (alexandria:compose
                             (add-folders-name->folder-id add-folders)
                             #'source-name)
                            sources)))
    (-> (add-folders-folder-dao add-folders)
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
(defun add (add-folders &key name thumbnail)
  (let ((id (funcall (add-folders-name->folder-id add-folders) name)))
    (-<> (add-folders-folder-dao add-folders)
         (cocoa.entity.folder:add-all
          (list (cocoa.entity.folder:make-folder-config
                 :id id
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
         (cocoa.use-case.folder:get-by-id id :folder-dao <>))))

