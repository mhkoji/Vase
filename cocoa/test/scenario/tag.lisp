(defpackage :cocoa.test.scenario.tag
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :cocoa.test.scenario.tag)

(defmacro change-the-name-of-a-tag (db &key test)
  `(progn
     (cocoa.tag:create ,db "tag")
     (cocoa.tag:change-name ,db "1" :name "tag (updated)")
     (let ((tags (cocoa.tag:list-by-range ,db 0 10)))
       (,test (equal tags '((:id "1" :name "tag (updated)")))))))
(export 'change-the-name-of-a-tag)

(defmacro delete-a-tag (db &key test)
  `(progn
     (cocoa.tag:create ,db "tag")
     (let ((tags (cocoa.tag:list-by-range ,db 0 10)))
       (,test (equal tags '((:id "1" :name "tag")))))

     (cocoa.tag:delete-by-id ,db "1")
     (,test (null (cocoa.tag:list-by-range ,db 0 10)))))
(export 'delete-a-tag)
