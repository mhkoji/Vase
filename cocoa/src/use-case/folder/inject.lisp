(defpackage :cocoa.use-case.folder.inject
  (:use :cl
        :cocoa.util.stream
        :cocoa.entity.folder))
(in-package :cocoa.use-case.folder.inject)
(cl-annot:enable-annot-syntax)

(defun add-images (paths &key image-factory image-repository)
  (let ((images (cocoa.entity.image:make-images/paths image-factory paths)))
    (cocoa.entity.image:save-images image-repository images)
    images))


(defclass simple-thumbnail ()
  ((thumbnail-id
    :initarg :thumbnail-id
    :reader thumbnail-id)))

(defun image->thumbnail (image)
  (make-instance 'simple-thumbnail
                 :thumbnail-id (cocoa.entity.image:image-id image)))

@export
(defun make-thumbnail (path &key image-factory image-repository)
  (image->thumbnail
   (car (add-images (list path)
                    :image-factory image-factory
                    :image-repository image-repository))))

@export
(defun thumbnail->image-id (thumbnail)
  (thumbnail-id thumbnail))


(defclass simple-content ()
  ((content-id
    :initarg :content-id
    :reader content-id)))

(defun image->content (image)
  (let ((content-id (format nil "image:~A"
                            (cocoa.entity.image:image-id image))))
    (make-instance 'simple-content :content-id content-id)))

@export
(defun content->image-id (content)
  (cl-ppcre:register-groups-bind (image-id)
      ("image:(.*)" (content-id content))
    image-id))

@export
(defun make-image-contents (paths &key image-factory image-repository)
  (mapcar #'image->content (add-images paths
                                       :image-factory image-factory
                                       :image-repository image-repository)))
