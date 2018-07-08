(defpackage :cocoa.spec.folder
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.spec.folder)
(cl-annot:enable-annot-syntax)

(defmethod cocoa.id:gen ((generator function) (string string))
  (funcall generator string))

@export
(defmacro can-list-the-added-folders (dao &key test)
  `(progn
     (cocoa.use-case.folder:add-bulk
      (list (cocoa.use-case.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100)
            (cocoa.use-case.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/ccc" "/path/f1/ddd")
             :modified-at 200))
      :id-generator
      (lambda (path)
        (subseq path (length "/path/")))
      :image-repository
      (cocoa.fs.image:image-repository ,dao)
      :folder-repository
      (cocoa.folder:folder-repository ,dao)
      :make-thumbnail-file
      (lambda (path)
        (format nil "~A:thumb" path)))
     (let ((folder (car (cocoa.use-case.folder:list-by-ids (list "f1")
                         :folder-repository
                         (cocoa.folder:folder-repository ,dao)))))
       (,test (string= (-> folder (getf :id))
                       "f1"))
       (,test (string= (-> folder (getf :name))
                       "/path/f1"))
       (,test (string= (-> folder (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))
     (let ((images (cocoa.use-case.folder:get-images "f1"
                    :from 0 :size 10
                    :folder-repository
                    (cocoa.folder:folder-repository ,dao))))
       (,test (= (length images) 2))
       (,test (string= (-> images (elt 0) (getf :id)) "f1/aaa"))
       (,test (string= (-> images (elt 1) (getf :id)) "f1/bbb")))))

@export
(defmacro can-attach-tags-to-a-folder (dao &key test)
  `(progn
     (cocoa.use-case.tag:create "A tag"
      :tag-repository (cocoa.tag:tag-repository ,dao))

     (cocoa.use-case.folder:add-bulk
      (list (cocoa.use-case.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100)
            (cocoa.use-case.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/ccc" "/path/f1/ddd")
             :modified-at 200))
      :id-generator
      (lambda (path)
        (subseq path (length "/path/")))
      :image-repository
      (cocoa.fs.image:image-repository ,dao)
      :folder-repository
      (cocoa.folder:folder-repository ,dao)
      :make-thumbnail-file
      (lambda (path)
        (format nil "~A:thumb" path)))

     (cocoa.use-case.folder:set-tags "f1" (list "1")
      :tag-repository (cocoa.tag:tag-repository ,dao))

     (let ((folders (cocoa.use-case.tag.contents:get-folders "1"
                     :tag-repository
                     (cocoa.tag:tag-repository ,dao)
                     :folder-repository
                     (cocoa.folder:folder-repository ,dao))))
       (,test (= (length folders) 1))
       (,test (string= (-> folders (elt 0) (getf :id))
                       "f1"))
       (,test (string= (-> folders (elt 0) (getf :name))
                       "/path/f1"))
       (,test (string= (-> folders (elt 0) (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))
     (let ((tags (cocoa.use-case.folder:get-tags "f1"
                  :tag-repository (cocoa.tag:tag-repository ,dao))))
       (,test (= (length tags) 1))
       (,test (string= (-> tags (elt 0) (getf :id))
                       "1"))
       (,test (string= (-> tags (elt 0) (getf :name))
                       "A tag")))))

