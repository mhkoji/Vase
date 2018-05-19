(defpackage :cocoa.infra.context
  (:use :cl))
(in-package :cocoa.infra.context)
(cl-annot:enable-annot-syntax)

@export
(defgeneric initialize (dao))

(defgeneric connection->dao (conn))

(defstruct context digest-fn connection-factory thumbnail-root)
(export 'make-context)
(export 'context-digest-fn)
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


@export
(defun sha256 (string)
  (ironclad:byte-array-to-hex-string
   (let ((octets (babel:string-to-octets string :encoding :utf-8)))
     (ironclad:digest-sequence 'ironclad:sha256 octets))))

@export
(defun sha256-3 (string)
  (subseq (sha256 (sha256 (sha256 string))) 0 10))



(defmethod connection->dao ((conn proton:sqlite3))
  (make-instance 'cocoa.infra.dao.sqlite3:sqlite3-dao :connection conn))

(defmethod initialize ((dao cocoa.infra.dao.sqlite3:sqlite3-dao))
  (cocoa.infra.dao.sqlite3:create-tables dao))
