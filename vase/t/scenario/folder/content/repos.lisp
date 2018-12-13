(defpackage :vase.t.scenario.folder.content.repos
  (:use :cl
        :vase.folder.content.repos))
(in-package :vase.t.scenario.folder.content.repos)

(defclass folder ()
  ((id
    :initarg :id
    :reader folder-id)))

(defclass content ()
  ((type
    :initarg :type
    :reader content-type)
   (entity-id
    :initarg :entity-id
    :reader content-entity-id)))

(defmethod vase.folder.content.entities.repos:bulk-load ((f function)
                                                         type
                                                         entity-ids)
  (funcall f type entity-ids))


(defmacro append-contents-then-bulk-load (db &key test)
  `(let ((folder (make-instance 'folder
                                :id "1234"))
         (contents (list (make-instance 'content
                                        :type :c
                                        :entity-id "5678"))))
     (bulk-append ,db (list (make-appending :folder folder
                                            :contents contents)))
     (let ((folder-contents
            (bulk-load (make-repository
                        :db ,db
                        :entity-repos (lambda (type entity-ids)
                                        (,test (eql type :c))
                                        (,test (equal entity-ids '("5678")))
                                        contents))
                       folder
                       :from 0
                       :size 1)))
       (mapc (lambda (folder-content content)
               (,test (eq folder-content content)))
             folder-contents contents))))
(export 'append-contents-then-bulk-load)
