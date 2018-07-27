(defpackage :cocoa.folder.folder-spec
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.folder.folder-spec)
(cl-annot:enable-annot-syntax)

(defmethod cocoa.entity.id:gen ((generator function) (string string))
  (funcall generator string))

(defmacro can-get-the-added-folder (db &key test)
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
      :db ,db
      :id-generator (lambda (path)
                      (subseq path (length "/path/")))
      :make-thumbnail-file (lambda (path)
                             (format nil "~A:thumb" path)))
     (let ((folder (cocoa.folder:get-folder "f1" :db ,db)))
       (,test (string= (-> folder (getf :id))
                       "f1"))
       (,test (string= (-> folder (getf :name))
                       "/path/f1"))
       (,test (string= (-> folder (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))))
(export 'can-get-the-added-folder)

(defmacro can-get-the-added-folder-images (db &key test)
  `(progn
     (cocoa.folder:add-bulk
      (list (cocoa.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100))
      :db ,db
      :id-generator (lambda (path)
                      (subseq path (length "/path/")))
      :make-thumbnail-file (lambda (path)
                             (format nil "~A:thumb" path)))
     (let ((images (cocoa.folder.content:get-images "f1"
                    :from 0 :size 10 :db ,db)))
       (,test (= (length images) 2))
       (,test (string= (-> images (elt 0) (getf :id)) "f1/aaa"))
       (,test (string= (-> images (elt 1) (getf :id)) "f1/bbb"))
       (let ((path (cocoa.image:get-path "f1/aaa" :db ,db)))
         (,test (string= path "/path/f1/aaa"))))))
(export 'can-get-the-added-folder-images)

(defmacro can-list-the-overviews-of-added-folders (db &key test)
  `(progn
     (cocoa.folder:add-bulk
      (list (cocoa.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa")
             :modified-at 100)
            (cocoa.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/bbb")
             :modified-at 200))
      :db ,db
      :id-generator (lambda (path)
                      (subseq path (length "/path/")))
      :make-thumbnail-file (lambda (path)
                             (format nil "~A:thumb" path)))
     (,test (equal (cocoa.folder:list-folder-overviews 0 10 :db ,db)
                   '((:id "f2"
                      :name "/path/f2"
                      :thumbnail (:id "f2/bbb:thumb"))
                     (:id "f1"
                      :name "/path/f1"
                      :thumbnail (:id "f1/aaa:thumb")))))))
(export 'can-list-the-overviews-of-added-folders)

(defmacro can-attach-tags-to-a-folder (db &key test)
  `(progn
     (cocoa.tag:create "A tag" :db ,db)

     (cocoa.folder:add-bulk
      (list (cocoa.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100)
            (cocoa.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/ccc" "/path/f1/ddd")
             :modified-at 200))
      :db ,db
      :id-generator (lambda (path)
                      (subseq path (length "/path/")))
      :make-thumbnail-file (lambda (path)
                             (format nil "~A:thumb" path)))

     (cocoa.folder:set-folder-tags "f1" (list "1") :db ,db)

     (let ((folders (cocoa.tag.contents:get-folders "1" :db ,db)))
       (,test (= (length folders) 1))
       (,test (string= (-> folders (elt 0) (getf :id))
                       "f1"))
       (,test (string= (-> folders (elt 0) (getf :name))
                       "/path/f1"))
       (,test (string= (-> folders (elt 0) (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))
     (let ((tags (cocoa.folder:get-folder-tags "f1" :db ,db)))
       (,test (= (length tags) 1))
       (,test (string= (-> tags (elt 0) (getf :id))
                       "1"))
       (,test (string= (-> tags (elt 0) (getf :name))
                       "A tag")))))
(export 'can-attach-tags-to-a-folder)
