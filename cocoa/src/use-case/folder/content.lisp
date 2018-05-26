(defpackage :cocoa.use-case.folder.content
  (:use :cl)
  (:import-from :cocoa.entity.folder.content
                :content-id))
(in-package :cocoa.use-case.folder.content)
(cl-annot:enable-annot-syntax)

(defclass simple-content ()
  ((content-id
    :initarg :content-id
    :reader content-id)))

;;; A image content implemented by some image id
;;; Implemented as a plug-in to the folder
@export
(defun make-of-image (image-id)
  (make-instance 'simple-content
                 :content-id (format nil "image:~A" image-id)))

@export
(defun content->image-id (content)
  (cl-ppcre:register-groups-bind (image-id)
      ("image:(.*)" (content-id content))
    image-id))
