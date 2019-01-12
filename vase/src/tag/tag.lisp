(defpackage :vase.tag
  (:use :cl)
  (:export :tag
           :tag-id
           :tag-name
           :tag-contents

           :content
           :content-id
           :content-type
           :content-tags
           :set-content-tags

           :save
           :update
           :bulk-load-by-ids
           :bulk-load-by-range
           :bulk-delete)
  (:import-from :vase.tag.contents
                :content-id
                :content-type))
(in-package :vase.tag)

(defclass tag ()
  ((id
    :initarg :id
    :reader tag-id)
   (name
    :initarg :name
    :accessor tag-name)))
