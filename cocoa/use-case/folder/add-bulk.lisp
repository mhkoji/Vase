(defpackage :cocoa.use-case.folder.add-bulk
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.use-case.folder.add-bulk)
(cl-annot:enable-annot-syntax)

;;; The representation of the source of a folder
(defstruct source name modified-at thumbnail contents)
(export 'make-source)

@export
(defun prepare (folder-repos name->folder-id)
  (let ((source->folder-id (alexandria:compose name->folder-id
                                               #'source-name)))
    (lambda (sources)
      (let ((folder-ids (mapcar source->folder-id sources)))
        (let ((folder-configs
               (mapcar (lambda (folder-id source)
                         (cocoa.folder:make-folder-config
                          :id folder-id
                          :name (source-name source)
                          :thumbnail (source-thumbnail source)
                          :modified-at (source-modified-at source)))
                       folder-ids sources))
              (appending-bulk
               (cocoa.folder:make-appending-bulk
                :appendings
                (mapcar (lambda (folder-id source)
                          (cocoa.folder:make-appending
                           :folder-id folder-id
                           :contents (source-contents source)))
                        folder-ids sources))))
          (-> folder-repos
              (cocoa.folder:save-folders folder-configs)
              (cocoa.folder:update-contents appending-bulk)))))))

@export
(defun exec (prepare sources)
  (funcall prepare sources))
