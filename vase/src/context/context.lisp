(defpackage :vase.context
  (:use :cl)
  (:export :with-context
           :db
           :id-gen
           :folder-repos
           :folder-content-repos
           :image-repos
           :tag-content-repos))
(in-package :vase.context)

(defstruct context db id-generator)

(defmacro with-context ((context) conf &body body)
  `(vase.context.configure:with-db (db ,conf)
     (let ((,context
            (make-context
             :db db
             :id-generator
             (vase.context.configure:configure-id-generator ,conf))))
       ,@body)))


(defun db (context)
  (context-db context))


(defun id-gen (context)
  (context-id-generator context))


(defun image-repos (context)
  (vase.image.repos:make-repository :db (context-db context)))


(defun folder-repos (context)
  (vase.folder.repos:make-repository
   :db (context-db context)
   :thumbnail-repos (image-repos context)))

(defun folder-content-repos (context)
  (vase.folder.content.repos:make-repository
   :db (db context)
   :entity-repos (image-repos context)))


(defun tag-content-repos (context)
  (folder-repos context))
