(defpackage :vase.folder.content
  (:use :cl)
  (:export :content
           :content-type
           :content-entity-id

           :bulk-delete
           :make-appending
           :bulk-append

           :bulk-load
           :bulk-load-by-folder

           :image
           :from-image))
(in-package :vase.folder.content)

(defclass content ()
  ((type :initarg :type
         :type keyword
         :reader content-type)
   (get-entity-id :initarg :get-entity-id
                  :reader get-entity-id)))

(defun content-entity-id (c)
  (funcall (get-entity-id c) c))


(defgeneric bulk-load (repos type entity-ids))
