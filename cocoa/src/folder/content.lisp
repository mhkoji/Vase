(defpackage :cocoa.folder.content
  (:use :cl)
  (:import-from :cocoa.entity.folder
                :content
                :content-id))
(in-package :cocoa.folder.content)
(cl-annot:enable-annot-syntax)

;;; A image content implemented by some image id
;;; Implemented as a plug-in to the folder
@export
(defun of-image (image-id)
  (make-instance 'content :id (format nil "image:~A" image-id)))

@export
(defun content->image-id (content)
  (cl-ppcre:register-groups-bind (image-id)
      ("image:(.*)" (content-id content))
    image-id))
