(defpackage :cocoa.use-case.folder.spec
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.use-case.folder.spec)
(cl-annot:enable-annot-syntax)

@export
(defmacro can-list-the-added-folders (folder-dao &key test)
  `(progn
     (cocoa.use-case.folder:add-bulk
      (list (cocoa.use-case.folder:make-dir
             :path "/path/f1"
             :file-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100)
            (cocoa.use-case.folder:make-dir
             :path "/path/f2"
             :file-paths (list "/path/f2/ccc" "/path/f1/ddd")
             :modified-at 200))
      :folder-repository
      (cocoa.folder:folder-repository ,folder-dao)
      :make-folder-id-by-path
      (lambda (path)
        (subseq path (length "/path/")))
      :make-thumbnail-file
      (lambda (path)
        (format nil "~A:thumb" path))
      :add-images-by-paths
      #'identity)
     (let ((folder (car (cocoa.use-case.folder:list-by-ids (list "f1")
                         :folder-repository
                         (cocoa.folder:folder-repository ,folder-dao)))))
       (,test (string= (-> folder (getf :id))
                       "f1"))
       (,test (string= (-> folder (getf :name))
                       "/path/f1"))
       (,test (string= (-> folder (getf :thumbnail) (getf :id))
                       "/path/f1/aaa:thumb")))))
