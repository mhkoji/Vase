(defpackage :cocoa.use-case.folder.add-bulk
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.use-case.folder.add-bulk)
(cl-annot:enable-annot-syntax)

;;; The representation of the source of a folder
(defstruct source name modified-at thumbnail contents)
(export 'make-source)

(defstruct add-bulk folder-dao name->folder-id)
(export 'make-add-bulk)

@export
(defun call (add-bulk sources)
  (let ((folder-ids (mapcar (alexandria:compose
                             (add-bulk-name->folder-id add-bulk)
                             #'source-name)
                            sources)))
    (-> (add-bulk-folder-dao add-bulk)
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


