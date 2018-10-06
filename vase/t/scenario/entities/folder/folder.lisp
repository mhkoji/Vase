(defpackage :vase.t.scenario.entities.folder
  (:use :cl
        :vase.entities.folder
        :vase.entities.folder.content)
  (:import-from :cl-arrows :->))
(in-package :vase.t.scenario.entities.folder)

(defmacro contains-contents (db &key test)
  `(let ((folder (make-folder
                  :id "1234"
                  :name "a folder name"
                  :thumbnail (make-thumbnail
                              "thumb:1234")
                  :modified-at 3736501114))
         (contents (list (make-content "c:5678"))))
     (let ((appending (vase.entities.folder.content.op:make-appending
                       :folder folder
                       :contents contents)))
       (-> ,db
           (vase.entities.folder.repository:save-bulk (list folder))
           (vase.entities.folder.content.repository:update appending)))
     (let ((loaded-folder
            (car (vase.entities.folder.repository:load-by-ids ,db
                  (list (folder-id folder))))))
       (mapc (lambda (folder-content content)
               (,test (string= (content-id folder-content)
                               (content-id content))))
             (vase.entities.folder.content.repository:folder-contents ,db
              loaded-folder :from 0 :size (length contents))
             contents))))
(export 'contains-contents)
