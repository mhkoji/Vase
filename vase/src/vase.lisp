(defpackage :vase
  (:use :cl))
(in-package :vase)
(cl-annot:enable-annot-syntax)

(defgeneric initialize (db))
(export 'initialize)

(defgeneric connection->db (conn))

(defstruct context id-generator connection-factory thumbnail-root)
(export 'make-context)
(export 'context-id-generator)
(export 'context-thumbnail-root)

(defmacro with-db ((db context &key domain) &body body)
  (let ((factory (gensym "FACTORY"))
        (callback (gensym "CALLBACK")))
    `(labels ((,callback (conn)
                (let ((,db (connection->db conn)))
                  ,@body)))
       (let ((,factory (context-connection-factory ,context)))
         (proton:call/connection ,factory #',callback
                                 :database-name (or ,domain "vase"))))))
(export 'with-db)

(defun load-context (&optional path)
  (unless path
    (setq path (merge-pathnames ".vase.config.lisp"
                                (user-homedir-pathname))))
  (when (cl-fad:file-exists-p path)
    (with-open-file (in path) (read in))))
(export 'load-context)


(defmethod connection->db ((conn proton:sqlite3))
  (make-instance 'vase.db.sqlite3:sqlite3-db :connection conn))

(defmethod initialize ((db vase.db.sqlite3:sqlite3-db))
  (vase.db.sqlite3:create-tables db))
