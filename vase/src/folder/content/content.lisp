(defpackage :vase.folder.content
  (:use :cl)
  (:export :content
           :content-type
           :content-entity-id
           :bulk-load
           :image
           :from-image))
(in-package :vase.folder.content)

(defclass content ()
  ((type :initarg :type
         :type keyword
         :reader content-type)
   (get-entity-id :initarg :get-entity-id
                  :reader get-entity-id)))

(defgeneric bulk-load (repos type entity-ids))
