(defpackage :vase.db.sqlite3
  (:use :cl :vase.db))
(in-package :vase.db.sqlite3)
(cl-annot:enable-annot-syntax)

@export
(defparameter +folders+
  "folders")
@export
(defparameter +folder-id+
  "folder_id")
@export
(defparameter +folders/name+
  "name")
@export
(defparameter +folders/modified-at+
  "modified_at")
@export
(defparameter +folder-thumbnails+
  "folder_thumbnails")
@export
(defparameter +thumbnail-id+
  "thumbnail_id")
@export
(defparameter +folder-contents+
  "folder_contents")
@export
(defparameter +content-id+
  "content_id")

@export
(defparameter +images+
  "images")
@export
(defparameter +image-id+
  "image_id")
@export
(defparameter +images/path+
  "path")


@export
(defclass sqlite3-db ()
  ((connection
    :initarg :connection
    :type proton:sqlite3
    :reader connection)))

@export
(defmacro query (db &rest args)
  `(proton:query (connection ,db) ,@args))

@export
(defmacro insert-bulk (db &rest args)
  `(proton:insert-bulk (connection ,db) ,@args))

@export
(defmacro delete-bulk (db &rest args)
  `(proton:delete-bulk (connection ,db) ,@args))

@export
(defmacro join (&rest args)
  `(proton:join ,@args))

@export
(defmacro placeholder (&rest args)
  `(proton:placeholder ,@args))

@export
(defun create-tables (db)
  (query db "DROP TABLE IF EXISTS folders;")
  (query db "CREATE TABLE folders (
              folder_id   varchar(256) NOT NULL UNIQUE,
              name        varchar(256) NOT NULL UNIQUE,
              modified_at datetime     CURRENT_TIMESTAMP
              );")
  (query db "DROP TABLE IF EXISTS folder_thumbnails;")
  (query db "CREATE TABLE folder_thumbnails (
              folder_id    varchar(256) NOT NULL UNIQUE,
              thumbnail_id varchar(256) NOT NULL UNIQUE
              );")
  (query db "DROP TABLE IF EXISTS folder_contents;")
  (query db "CREATE TABLE folder_contents (
              folder_id  varchar(256) NOT NULL,
              content_id varchar(256) NOT NULL UNIQUE
              );")

  (query db "DROP TABLE IF EXISTS images;")
  (query db "CREATE TABLE images (
               image_id varchar(256) NOT NULL UNIQUE,
               path     varchar(256) NOT NULL UNIQUE
              );")

  (query db "DROP TABLE IF EXISTS tags;")
  (query db "CREATE TABLE tags (
              tag_id     INTEGER      PRIMARY KEY AUTOINCREMENT,
              name       varchar(256) NOT NULL,
              created_at datetime     CURRENT_TIMESTAMP
              );")
  (query db "DROP TABLE IF EXISTS tag_contents;")
  (query db "CREATE TABLE   tag_contents (
              id           INTEGER      PRIMARY KEY AUTOINCREMENT,
              tag_id       varchar(256) NOT NULL,
              content_id   varchar(256) NOT NULL,
              content_type varchar(256) NOT NULL
              );")
  )


(defmethod initialize ((db sqlite3-db))
  (create-tables db))


(defmethod connection->db ((conn proton:sqlite3))
  (make-instance 'sqlite3-db :connection conn))
