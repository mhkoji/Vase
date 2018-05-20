(defpackage :cocoa.use-case.folder.inject
  (:use :cl
        :cocoa.util.stream
        :cocoa.entity.folder))
(in-package :cocoa.use-case.folder.inject)
(cl-annot:enable-annot-syntax)

(defun image-id (image)
  (getf image :id))


(defclass image-thumbnail ()
  ((thumbnail-id
    :initarg :thumbnail-id
    :reader thumbnail-id)))

@export
(defun make-thumbnail (path &key image-factory image-repository)
  (let ((image (car (cocoa.use-case.image:add-images (list path)
                     :image-factory image-factory
                     :image-repository image-repository))))
    (make-instance 'image-thumbnail :thumbnail-id (image-id image))))

@export
(defun thumbnail->image-id (thumbnail)
  (thumbnail-id thumbnail))


(defclass image-content ()
  ((content-id :reader content-id)))

(defmethod initialize-instance :after ((content image-content)
                                       &key image-id)
  (setf (slot-value content 'content-id) (format nil "image:~A" image-id)))

@export
(defun content->image-id (content)
  (cl-ppcre:register-groups-bind (image-id)
      ("image:(.*)" (content-id content))
    image-id))

@export
(defun make-image-contents (paths &key image-factory image-repository)
  (mapcar (lambda (image) (make-instance 'image-content
                                         :image-id (image-id image)))
          (cocoa.use-case.image:add-images paths
           :image-factory image-factory
           :image-repository image-repository)))
