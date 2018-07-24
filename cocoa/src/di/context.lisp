(defpackage :cocoa.di.context
  (:use :cl))
(in-package :cocoa.di.context)
(cl-annot:enable-annot-syntax)

@export
(defgeneric initialize (db))

(defgeneric connection->db (conn))

(defstruct context id-generator connection-factory thumbnail-root)
(export 'make-context)
(export 'context-id-generator)
(export 'context-thumbnail-root)

@export
(defmacro with-db ((db context &key domain) &body body)
  (let ((factory (gensym "FACTORY"))
        (callback (gensym "CALLBACK")))
    `(labels ((,callback (conn)
                (let ((,db (connection->db conn)))
                  ,@body)))
       (let ((,factory (context-connection-factory ,context)))
         (proton:call/connection ,factory #',callback
                                 :database-name (or ,domain "cocoa"))))))

@export
(defun load-context (&optional path)
  (unless path
    (setq path (merge-pathnames ".cocoa.config.lisp"
                                (user-homedir-pathname))))
  (when (cl-fad:file-exists-p path)
    (with-open-file (in path) (read in))))


(defmethod connection->db ((conn proton:sqlite3))
  (make-instance 'cocoa.db.sqlite3:sqlite3-db :connection conn))

(defmethod initialize ((db cocoa.db.sqlite3:sqlite3-db))
  (cocoa.db.sqlite3:create-tables db))
