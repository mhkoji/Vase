(defpackage :vase.entities.folder
  (:use :cl))
(in-package :vase.entities.folder)

(defstruct thumbnail image-id url)

(defstruct folder id name thumbnail)
