(defpackage :cocoa.tag.tag-spec
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.tag.tag-spec)

(defmacro can-change-the-name-of-a-tag (db &key test)
  `(progn
     (cocoa.tag:create "tag" :db ,db)
     (cocoa.tag:change-name "1" "tag (updated)" :db ,db)
     (let ((tags (cocoa.tag:list-by-range 0 10 :db ,db)))
       (,test (equal tags '((:id "1" :name "tag (updated)")))))))
(export 'can-change-the-name-of-a-tag)

(defmacro can-delete-a-tag (db &key test)
  `(progn
     (cocoa.tag:create "tag" :db ,db)
     (let ((tags (cocoa.tag:list-by-range 0 10 :db ,db)))
       (,test (equal tags '((:id "1" :name "tag")))))

     (cocoa.tag:delete-by-id "1" :db ,db)
     (,test (null (cocoa.tag:list-by-range 0 10 :db ,db)))))
(export 'can-delete-a-tag)
