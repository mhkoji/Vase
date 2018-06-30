(defpackage :cocoa.use-case.folder.spec
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.use-case.folder.spec)
(cl-annot:enable-annot-syntax)

@export
(defmacro can-list-the-added-folders (folder-dao &key test)
  `(progn
     (-> (cocoa.use-case.folder:add-bulk
          (cocoa.folder:folder-repository ,folder-dao) #'identity)
         (funcall
          (list (cocoa.use-case.folder:make-source
                 :name "folder-1"
                 :modified-at 100
                 :thumbnail (cocoa.use-case.folder.thumbnail:make-of-image
                             "thumb:1"))
                (cocoa.use-case.folder:make-source
                 :name "folder-2"
                 :modified-at 200
                 :thumbnail (cocoa.use-case.folder.thumbnail:make-of-image
                             "thumb:2")))))
     (let ((folder (car (-> (cocoa.use-case.folder:list-by-ids
                             (cocoa.folder:folder-repository ,folder-dao))
                            (funcall (list "folder-1"))))))
       (,test (string= (-> folder (getf :id))
                       "folder-1"))
       (,test (string= (-> folder (getf :name))
                       "folder-1"))
       (,test (string= (-> folder (getf :thumbnail) (getf :id))
                       "thumb:1")))))
