(defpackage :cocoa.entity.folder.folder-spec
  (:use :cl
        :cocoa.entity.folder
        :cocoa.entity.folder.content)
  (:import-from :cl-arrows :->))
(in-package :cocoa.entity.folder.folder-spec)
(cl-annot:enable-annot-syntax)

@export
(defun folder-can-contain-contents (db)
  (let ((folder (make-folder
                 :id "1234"
                 :name "a folder name"
                 :thumbnail (make-thumbnail
                             "thumb:1234")
                 :modified-at 3736501114))
        (contents (list (make-content "c:5678"))))
    (let ((appending (cocoa.entity.folder.content.op:make-appending
                      :folder folder
                      :contents contents)))
      (-> db
        (cocoa.entity.folder.repository:save-bulk (list folder))
        (cocoa.entity.folder.content.repository:update appending)))
    (let ((loaded-folder
           (car (cocoa.entity.folder.repository:load-by-ids db
                 (list (folder-id folder))))))
      (every (lambda (folder-content content)
               (string= (content-id folder-content)
                        (content-id content)))
             (cocoa.entity.folder.content.repository:folder-contents
              db loaded-folder :from 0 :size (length contents))
             contents))))
