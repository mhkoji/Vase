(defpackage :vase.t.scenario.tag
  (:use :cl)
  (:import-from :cl-arrows :->))
(in-package :vase.t.scenario.tag)

(defmacro change-the-name-of-a-tag (db &key test)
  `(progn
     (vase.app.tag:create ,db "tag")
     (vase.app.tag:change-name ,db "1" :name "tag (updated)")
     (let ((tags (vase.app.tag:list-by-range ,db 0 10)))
       (,test (equal tags '((:id "1" :name "tag (updated)")))))))
(export 'change-the-name-of-a-tag)

(defmacro delete-a-tag (db &key test)
  `(progn
     (vase.app.tag:create ,db "tag")
     (let ((tags (vase.app.tag:list-by-range ,db 0 10)))
       (,test (equal tags '((:id "1" :name "tag")))))

     (vase.app.tag:delete-by-id ,db "1")
     (,test (null (vase.app.tag:list-by-range ,db 0 10)))))
(export 'delete-a-tag)
