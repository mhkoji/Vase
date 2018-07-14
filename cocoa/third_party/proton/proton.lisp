(defpackage proton
  (:use :cl))
(in-package :proton)
(cl-annot:enable-annot-syntax)
(cl-interpol:enable-interpol-syntax)

@export
(defgeneric call/connection (conn-factory callback &key database-name))
@export
(defgeneric query (conn sql &optional args))
@export
(defgeneric insert-bulk (conn table columns rows))
@export
(defgeneric delete-bulk (conn table column values))

@export
(defun mklist (len init-elem)
  (make-list len :initial-element init-elem))

@export
(defun placeholder (list)
  (let ((list (mklist (length list) "?")))
    (format nil "~A~{,~A~}" (car list) (cdr list))))

@export
(defun join (&rest strings)
  (format nil "~{~A ~}" strings))


(defun insert-bulk-query (table columns row-length)
  (let ((cl-interpol:*list-delimiter* ","))
    (let ((holder #?{(@{(mklist (length columns) "?")})}))
      #?{INSERT INTO ${table}
           (@{columns})
         VALUES
           @{(mklist row-length holder)}})))

(defun concat-lists (lists)
  (let ((concated nil))
    (dolist (list (reverse lists))
      (setq concated (append list concated)))
    concated))

(defmethod insert-bulk ((conn t) table columns rows)
  (when rows
    (query conn (insert-bulk-query table columns (length rows))
           (concat-lists rows))))


(defun delete-bulk-query (table column values)
  (let ((cl-interpol:*list-delimiter* ","))
    #?{DELETE FROM
         ${table}
       WHERE
         ${column} IN (@{(mklist (length values) "?")})}))

(defmethod delete-bulk ((conn t) table column values)
  (when values
    (query conn (delete-bulk-query table column values) values)))


;;; sql
(defmacro with-release ((conn) &body body)
  `(unwind-protect
        (progn ,@body)
     (ignore-errors (dbi:disconnect ,conn))))

;;;; sqlite3
@export
(defclass sqlite3 ()
  ((connection
    :initarg :connection
    :reader connection)))

(defmethod query ((sqlite3 sqlite3) sql &optional args)
  (let ((query (dbi:prepare (connection sqlite3) sql)))
    (unwind-protect
         (dbi:fetch-all (apply #'dbi:execute query args))
      (sqlite:finalize-statement (dbi.driver:query-prepared query)))))

(defun make-sqlite3 (conn)
  (make-instance 'sqlite3 :connection conn))

@export
(defclass sqlite3-factory ()
  ((db-path
    :initarg :db-path
    :reader sqlite3-factory-db-path)))

(defmethod call/connection ((factory sqlite3-factory) callback
                            &key database-name)
  (declare (ignore database-name))
  (let ((conn (dbi:connect :sqlite3
               :busy-timeout 100
               :database-name (sqlite3-factory-db-path factory))))
    (with-release (conn)
      (dbi:with-transaction conn
        (funcall callback (make-sqlite3 conn))))))


@export
(defclass in-memory-sqlite3-factory ()
  ((connection
    :initarg :connection
    :initform nil
    :accessor in-memory-sqlite3-factory-connection)
   (mutex
    :reader in-memory-sqlite3-factory-mutex
    :initform
    #+ccl
    (ccl:make-lock "in-memory-sqlite3")
    #+sbcl
    (sb-thread:make-mutex :name "in-memory-sqlite3"))))

(defun in-memory-sqlite3-call/transaction (conn mutex callback)
  #+ccl
  (ccl:with-lock-grabbed (mutex)
    (dbi:with-transaction conn
      (funcall callback conn)))
  #+sbcl
  (sb-thread:with-mutex (mutex)
    (dbi:with-transaction conn
      (funcall callback conn))))

(defmethod call/connection ((factory in-memory-sqlite3-factory) callback
                            &key database-name)
  (declare (ignore database-name))
  (with-accessors ((conn in-memory-sqlite3-factory-connection)) factory
    (unless conn
      (setf conn (dbi:connect :sqlite3
                  :busy-timeout 100
                  :database-name ":memory:"))))
  (in-memory-sqlite3-call/transaction
   (in-memory-sqlite3-factory-connection factory)
   (in-memory-sqlite3-factory-mutex factory)
   (alexandria:compose callback #'make-sqlite3)))

;;;; postgresql
@export
(defclass postgresql ()
  ((connection
    :initarg :connection
    :reader connection)))

(defmethod query ((postgresql postgresql) sql &optional args)
  (let ((query (dbi:prepare (connection postgresql) sql)))
    (dbi:fetch-all (apply #'dbi:execute query args))))

(defun make-postgresql (conn)
  (make-instance 'postgresql :connection conn))

@export
(defclass postgresql-factory ()
  ((username
    :initarg :username
    :reader postgresql-factory-username)))

(defmethod call/connection ((factory postgresql-factory) callback
                            &key database-name)
  (let ((conn (dbi:connect :postgres
               :username (postgresql-factory-username factory)
               :database-name database-name)))
    (with-release (conn)
      (dbi:with-transaction conn
        (funcall callback (make-postgresql conn))))))
