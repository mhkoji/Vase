(defpackage :vase.app.container
  (:use :cl)
  (:export :make-configure
           :load-configure
           :configure-thumbnail-root

           :with-container
           :container-db
           :container-id-generator
           :container-image-repository
           :container-folder-repository))
(in-package :vase.app.container)

(defstruct configure
  id-generator
  connection-factory
  thumbnail-root)

(defun load-configure (&optional path)
  (unless path
    (setq path (merge-pathnames ".vase.config.lisp"
                                (user-homedir-pathname))))
  (when (cl-fad:file-exists-p path)
    (with-open-file (in path) (read in))))


(defstruct container db id-generator)

(defun container-image-repository (container)
  (vase.image.repos:make-repository
   :db (container-db container)))

(defun container-folder-repository (container)
  (vase.folder.repos:make-repository
   :db (container-db container)
   :thumbnail-repos (container-image-repository container)))

(defmacro with-db ((db connection-factory &key domain) &body body)
  (let ((callback (gensym "CALLBACK")))
    `(labels ((,callback (conn)
                (let ((,db (vase.db:connection->db conn)))
                  ,@body)))
       (proton:call/connection ,connection-factory
                               #',callback
                               :database-name (or ,domain "vase")))))

(defmacro with-container ((container conf &key domain) &body body)
  `(with-db (db (configure-connection-factory ,conf)
                :domain ,domain)
     (let ((,container
            (make-container
             :db db
             :id-generator (configure-id-generator ,conf))))
       ,@body)))
