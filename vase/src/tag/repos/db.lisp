(defpackage :vase.tag.repos.db
  (:use :cl)
  (:shadow :delete)
  (:export :make-row
           :row
           :row-tag-id
           :row-name
           :make-content-row
           :content-row
           :content-row-id
           :content-row-type
           :insert
           :delete
           :update
           :select-by-ids
           :select-by-range
           :content/insert
           :content/delete
           :content/select
           :content/select-tags))
(in-package :vase.tag.repos.db)

(defstruct row tag-id name)
(defgeneric insert (db name))
(defgeneric delete (db tag-id-list))
(defgeneric update (db tag-row))
(defgeneric select-by-ids (db ids))
(defgeneric select-by-range (db offset size))

(defstruct content-row id type)
(defgeneric content/insert (db content-row tag-id-list))
(defgeneric content/delete (db content-row tag-id-list))
(defgeneric content/select (db tag-id))
(defgeneric content/select-tags (db content-row))
