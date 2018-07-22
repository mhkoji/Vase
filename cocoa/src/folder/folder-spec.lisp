(defpackage :cocoa.folder.folder-spec
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.folder.folder-spec)
(cl-annot:enable-annot-syntax)

(defmethod cocoa.entity.id:gen ((generator function) (string string))
  (funcall generator string))

@export
(defmacro can-get-the-added-folder (dao &key test)
  `(progn
     (cocoa.folder:add-bulk
      (list (cocoa.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100)
            (cocoa.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/ccc" "/path/f1/ddd")
             :modified-at 200))
      :id-generator
      (lambda (path)
        (subseq path (length "/path/")))
      :image-repository
      (cocoa.entity.fs.image:image-repository ,dao)
      :folder-repository
      (cocoa.entity.folder:folder-repository ,dao)
      :folder-content-repository
      (cocoa.entity.folder:folder-content-repository ,dao)
      :make-thumbnail-file
      (lambda (path)
        (format nil "~A:thumb" path)))
     (let ((folder (cocoa.folder:get-folder "f1"
                    :folder-repository
                    (cocoa.entity.folder:folder-repository ,dao))))
       (,test (string= (-> folder (getf :id))
                       "f1"))
       (,test (string= (-> folder (getf :name))
                       "/path/f1"))
       (,test (string= (-> folder (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))
     (let ((images (cocoa.folder.content:get-images "f1"
                    :from 0 :size 10
                    :folder-repository
                    (cocoa.entity.folder:folder-repository ,dao)
                    :folder-content-repository
                    (cocoa.entity.folder:folder-content-repository ,dao))))
       (,test (= (length images) 2))
       (,test (string= (-> images (elt 0) (getf :id)) "f1/aaa"))
       (,test (string= (-> images (elt 1) (getf :id)) "f1/bbb")))))

@export
(defmacro can-attach-tags-to-a-folder (dao &key test)
  `(progn
     (cocoa.tag:create "A tag"
      :tag-repository (cocoa.entity.tag:tag-repository ,dao))

     (cocoa.folder:add-bulk
      (list (cocoa.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100)
            (cocoa.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/ccc" "/path/f1/ddd")
             :modified-at 200))
      :id-generator
      (lambda (path)
        (subseq path (length "/path/")))
      :image-repository
      (cocoa.entity.fs.image:image-repository ,dao)
      :folder-repository
      (cocoa.entity.folder:folder-repository ,dao)
      :folder-content-repository
      (cocoa.entity.folder:folder-content-repository ,dao)
      :make-thumbnail-file
      (lambda (path)
        (format nil "~A:thumb" path)))

     (cocoa.folder:set-folder-tags "f1" (list "1")
      :tag-repository (cocoa.entity.tag:tag-repository ,dao))

     (let ((folders (cocoa.tag.contents:get-folders "1"
                     :tag-repository
                     (cocoa.entity.tag:tag-repository ,dao)
                     :folder-repository
                     (cocoa.entity.folder:folder-repository ,dao))))
       (,test (= (length folders) 1))
       (,test (string= (-> folders (elt 0) (getf :id))
                       "f1"))
       (,test (string= (-> folders (elt 0) (getf :name))
                       "/path/f1"))
       (,test (string= (-> folders (elt 0) (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))
     (let ((tags (cocoa.folder:get-folder-tags "f1"
                  :tag-repository
                  (cocoa.entity.tag:tag-repository ,dao))))
       (,test (= (length tags) 1))
       (,test (string= (-> tags (elt 0) (getf :id))
                       "1"))
       (,test (string= (-> tags (elt 0) (getf :name))
                       "A tag")))))

