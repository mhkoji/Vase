(defpackage :cocoa.test.scenario.entity.folder
  (:use :cl
        :cocoa.entity.folder
        :cocoa.entity.folder.content)
  (:import-from :cl-arrows :->))
(in-package :cocoa.test.scenario.entity.folder)

(defmacro contains-contents (db &key test)
  `(let ((folder (make-folder
                  :id "1234"
                  :name "a folder name"
                  :thumbnail (make-thumbnail
                              "thumb:1234")
                  :modified-at 3736501114))
         (contents (list (make-content "c:5678"))))
     (let ((appending (cocoa.entity.folder.content.op:make-appending
                       :folder folder
                       :contents contents)))
       (-> ,db
           (cocoa.entity.folder.repository:save-bulk (list folder))
           (cocoa.entity.folder.content.repository:update appending)))
     (let ((loaded-folder
            (car (cocoa.entity.folder.repository:load-by-ids ,db
                  (list (folder-id folder))))))
       (mapc (lambda (folder-content content)
               (,test (string= (content-id folder-content)
                               (content-id content))))
             (cocoa.entity.folder.content.repository:folder-contents ,db
              loaded-folder :from 0 :size (length contents))
             contents))))
(export 'contains-contents)
