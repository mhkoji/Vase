(defpackage :cocoa.entity.folder.folder-spec
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.entity.folder.folder-spec)
(cl-annot:enable-annot-syntax)

@export
(defun folder-can-contain-contents (db)
  (let ((folder (cocoa.entity.folder:make-folder
                 :id "1234"
                 :name "a folder name"
                 :thumbnail (cocoa.entity.folder:make-thumbnail
                             "thumb:1234")
                 :modified-at 3736501114))
        (contents (list (cocoa.entity.folder.content:make-content
                         "c:5678"))))
    (-> db
        (cocoa.entity.folder.repository:save-bulk
         (list folder))
        (cocoa.entity.folder.content.repository:update
         (cocoa.entity.folder.content.op:make-appending
          :folder folder :contents contents)))
    (let ((loaded-folder
           (car (cocoa.entity.folder.repository:load-by-ids
                 db
                 (list (cocoa.entity.folder:folder-id folder))))))
      (every (lambda (folder-content content)
               (string= (cocoa.entity.folder.content:content-id
                         folder-content)
                        (cocoa.entity.folder.content:content-id
                         content)))
             (cocoa.entity.folder.content.repository:folder-contents
              db loaded-folder :from 0 :size (length contents))
             contents))))
