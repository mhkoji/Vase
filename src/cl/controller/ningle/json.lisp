(defpackage :cocoa.controller.ningle.json
  (:use :cl))
(in-package :cocoa.controller.ningle.json)
(cl-annot:enable-annot-syntax)

@export
(defun array-of (by elements)
  (mapcar by elements))

@export
(defun image (plist)
  (let ((id (getf plist :id)))
    (jsown:new-js
      ("image-id" id)
      ("url" (format nil "/_i/~A" id)))))

@export
(defun folder (plist)
  (jsown:new-js
    ("folder-id" (getf plist :id))
    ("name" (getf plist :name))
    ("thumbnail" (image (getf plist :thumbnail)))))

@export
(defun tag (plist)
  (jsown:new-js
    ("tag-id" (getf plist :id))
    ("name" (getf plist :name))))
