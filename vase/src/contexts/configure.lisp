(defpackage :vase.contexts.configure
  (:use :cl)
  (:export :make-configure
           :configure-id-generator
           :configure-thumbnail-root
           :with-db
           :initialize-db
           :load-configure))
(in-package :vase.contexts.configure)

(defgeneric initialize-db (db))

(defgeneric connection->db (conn))

(defstruct configure
  id-generator
  connection-factory
  thumbnail-root)

(defmacro with-db ((db configure &key domain) &body body)
  (let ((factory (gensym "FACTORY"))
        (callback (gensym "CALLBACK")))
    `(labels ((,callback (conn)
                (let ((,db (connection->db conn)))
                  ,@body)))
       (let ((,factory (configure-connection-factory ,configure)))
         (proton:call/connection ,factory #',callback
                                 :database-name (or ,domain "vase"))))))

(defun load-configure (&optional path)
  (unless path
    (setq path (merge-pathnames ".vase.config.lisp"
                                (user-homedir-pathname))))
  (when (cl-fad:file-exists-p path)
    (with-open-file (in path) (read in))))


(defmethod connection->db ((conn proton:sqlite3))
  (make-instance 'vase.db.sqlite3:sqlite3-db :connection conn))

(defmethod initialize-db ((db vase.db.sqlite3:sqlite3-db))
  (vase.db.sqlite3:create-tables db))
