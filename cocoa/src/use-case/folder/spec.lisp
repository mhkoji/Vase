(defpackage :cocoa.use-case.folder.spec
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.use-case.folder.spec)
(cl-annot:enable-annot-syntax)

@export
(defmacro can-list-the-added-folders (folder-dao &key test)
  `(progn
     (cocoa.use-case.folder:add-by-sources
      (list (cocoa.use-case.folder:make-source
             :name "folder-1"
             :modified-at 100
             :thumbnail (cocoa.use-case.folder.thumbnail:make-of-image
                         "thumb:1"))
            (cocoa.use-case.folder:make-source
             :name "folder-2"
             :modified-at 200
             :thumbnail (cocoa.use-case.folder.thumbnail:make-of-image
                         "thumb:2")))
      :folder-dao ,folder-dao
      :name->folder-id #'identity)
     (let ((folder (car (cocoa.use-case.folder:list/ids (list "folder-1")
                         :folder-dao ,folder-dao))))
       (,test (string= (-> folder (getf :id))
                       "folder-1"))
       (,test (string= (-> folder (getf :name))
                       "folder-1"))
       (,test (string= (-> folder (getf :thumbnail) (getf :id))
                       "thumb:1")))))
