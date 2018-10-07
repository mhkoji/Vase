(defpackage :vase.entities.tag-edit
  (:use :cl))
(in-package :vase.entities.tag)

(defstruct tag-edit
  folder-id
  tags
  attached-tags)
