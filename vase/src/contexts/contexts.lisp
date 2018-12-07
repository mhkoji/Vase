(defpackage :vase.contexts
  (:use :cl)
  (:export :with-folder
           :with-image
           :with-tag))
(in-package :vase.contexts)

(defmacro with-folder (((repos factory) conf) &body body)
  `(vase.contexts.configure:with-db (db ,conf)
     (let ((,repos
            (vase.folder:make-repository
             :db db
             :content-repos (vase.image:make-repository :db db)
             :thumbnail-repos (vase.image:make-repository :db db)))
           (,factory
            (vase.folder:make-factory
             :id-generator
             (vase.contexts.configure:configure-id-generator ,conf))))
       ,@body)))


(defmacro with-image (((repos factory) conf) &body body)
  `(vase.contexts.configure:with-db (db ,conf)
     (let ((,repos
            (vase.image:make-repository :db db))
           (,factory
            (vase.image:make-factory
             :id-generator
             (vase.contexts.configure:configure-id-generator ,conf))))
       ,@body)))


(defmacro with-tag (((repos) conf) &body body)
  `(vase.contexts.configure:with-db (db ,conf)
     (let ((,repos
            (vase.tag:make-repository
             :db db
             :content-repos
             (vase.folder:make-repository
              :db db
              :content-repos (vase.image:make-repository :db db)
              :thumbnail-repos (vase.image:make-repository :db db)))))
       ,@body)))
