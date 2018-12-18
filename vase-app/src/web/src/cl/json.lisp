(defpackage :vase.app.web.json
  (:use :cl)
  (:export :convert))
(in-package :vase.app.web.json)

(defgeneric convert (obj))

(defmethod convert ((obj list))
  (mapcar #'convert obj))

(defmethod convert ((obj vase.image:image))
  (let ((image-id (vase.image:image-id obj)))
    (jsown:new-js
     ("image-id" image-id)
     ("url" (format nil "/_i/~A" image-id)))))

(defmethod convert ((obj vase.folder:folder))
  (jsown:new-js
    ("folder-id" (vase.folder:folder-id obj))
    ("name" (vase.folder:folder-name obj))
    ("thumbnail" (convert (vase.folder:folder-thumbnail obj)))))

(defmethod convert ((obj vase.tag:tag))
  (jsown:new-js
    ("tag-id" (vase.tag:tag-id obj))
    ("name" (vase.tag:tag-name obj))))
