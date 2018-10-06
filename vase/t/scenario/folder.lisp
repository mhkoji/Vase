(defpackage :vase.t.scenario.folder
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :vase.t.scenario.folder)

(defmethod vase.entities.id:gen ((generator function) (string string))
  (funcall generator string))

(defvar *id-generator*
  (lambda (path)
    (subseq path (length "/path/"))))

(defvar *make-thumbnail-file-fn*
  (lambda (path)
    (format nil "~A:thumb" path)))

(defmacro get-the-added-folder (db &key test)
  `(progn
     (vase.app.folder:add-bulk ,db
      (list (vase.app.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100)
            (vase.app.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/ccc" "/path/f1/ddd")
             :modified-at 200))
      :id-generator *id-generator*
      :make-thumbnail-file-fn *make-thumbnail-file-fn*)
     (let ((folder (vase.app.folder:get-folder ,db "f1")))
       (,test (string= (-> folder (getf :id))
                       "f1"))
       (,test (string= (-> folder (getf :name))
                       "/path/f1"))
       (,test (string= (-> folder (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))))
(export 'get-the-added-folder)

(defmacro get-the-added-folder-images (db &key test)
  `(progn
     (vase.app.folder:add-bulk ,db
      (list (vase.app.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100))
      :id-generator *id-generator*
      :make-thumbnail-file-fn *make-thumbnail-file-fn*)
     (let ((images (vase.app.folder.content:get-images ,db "f1"
                    :from 0 :size 10)))
       (,test (= (length images) 2))
       (,test (string= (-> images (elt 0) (getf :id)) "f1/aaa"))
       (,test (string= (-> images (elt 1) (getf :id)) "f1/bbb"))
       (let ((path (vase.app.image:get-path ,db "f1/aaa")))
         (,test (string= path "/path/f1/aaa"))))))
(export 'get-the-added-folder-images)

(defmacro list-the-overviews-of-added-folders (db &key test)
  `(progn
     (vase.app.folder:add-bulk ,db
      (list (vase.app.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa")
             :modified-at 100)
            (vase.app.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/bbb")
             :modified-at 200))
      :id-generator *id-generator*
      :make-thumbnail-file-fn *make-thumbnail-file-fn*)
     (,test (equal (vase.app.folder:list-folder-overviews ,db 0 10)
                   '((:id "f2"
                      :name "/path/f2"
                      :thumbnail (:id "f2/bbb:thumb"))
                     (:id "f1"
                      :name "/path/f1"
                      :thumbnail (:id "f1/aaa:thumb")))))))
(export 'list-the-overviews-of-added-folders)

(defmacro attach-tags-to-a-folder (db &key test)
  `(progn
     (vase.app.tag:create ,db "A tag")

     (vase.app.folder:add-bulk ,db
      (list (vase.app.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100)
            (vase.app.folder:make-dir
             :path "/path/f2"
             :image-paths (list "/path/f2/ccc" "/path/f1/ddd")
             :modified-at 200))
      :id-generator *id-generator*
      :make-thumbnail-file-fn *make-thumbnail-file-fn*)

     (vase.app.folder:set-folder-tags ,db "f1" (list "1"))

     (let ((folders (vase.app.tag.contents:get-folders ,db "1")))
       (,test (= (length folders) 1))
       (,test (string= (-> folders (elt 0) (getf :id))
                       "f1"))
       (,test (string= (-> folders (elt 0) (getf :name))
                       "/path/f1"))
       (,test (string= (-> folders (elt 0) (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))
     (let ((tags (vase.app.folder:get-folder-tags ,db "f1")))
       (,test (= (length tags) 1))
       (,test (string= (-> tags (elt 0) (getf :id))
                       "1"))
       (,test (string= (-> tags (elt 0) (getf :name))
                       "A tag")))))
(export 'attach-tags-to-a-folder)

(defmacro change-tags-attached-a-folder (db &key test)
  `(progn
     (vase.app.tag:create ,db "A tag")

     (vase.app.folder:add-bulk ,db
      (list (vase.app.folder:make-dir
             :path "/path/f1"
             :image-paths (list "/path/f1/aaa" "/path/f1/bbb")
             :modified-at 100))
      :id-generator *id-generator*
      :make-thumbnail-file-fn *make-thumbnail-file-fn*)

     (vase.app.folder:set-folder-tags ,db "f1" (list "1"))

     (vase.app.tag:create ,db "Another tag")

     (vase.app.folder:set-folder-tags ,db "f1" (list "2"))

     (let ((folders (vase.app.tag.contents:get-folders ,db "2")))
       (,test (= (length folders) 1))
       (,test (string= (-> folders (elt 0) (getf :id))
                       "f1"))
       (,test (string= (-> folders (elt 0) (getf :name))
                       "/path/f1"))
       (,test (string= (-> folders (elt 0) (getf :thumbnail) (getf :id))
                       "f1/aaa:thumb")))
     (let ((tags (vase.app.folder:get-folder-tags ,db "f1")))
       (,test (= (length tags) 1))
       (,test (string= (-> tags (elt 0) (getf :id))
                       "2"))
       (,test (string= (-> tags (elt 0) (getf :name))
                       "Another tag")))))
(export 'change-tags-attached-a-folder)
