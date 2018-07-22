(defpackage :cocoa.di.context
  (:use :cl))
(in-package :cocoa.di.context)
(cl-annot:enable-annot-syntax)

@export
(defgeneric initialize (dao))

(defgeneric connection->dao (conn))

(defstruct context id-generator connection-factory thumbnail-root)
(export 'make-context)
(export 'context-id-generator)
(export 'context-thumbnail-root)

@export
(defmacro with-dao ((dao context &key domain) &body body)
  (let ((factory (gensym "FACTORY"))
        (callback (gensym "CALLBACK")))
    `(labels ((,callback (conn)
                (let ((,dao (connection->dao conn)))
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


(defmethod connection->dao ((conn proton:sqlite3))
  (make-instance 'cocoa.db.sqlite3:sqlite3-dao :connection conn))

(defmethod initialize ((dao cocoa.db.sqlite3:sqlite3-dao))
  (cocoa.db.sqlite3:create-tables dao))
