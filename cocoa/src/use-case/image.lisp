(defpackage :cocoa.use-case.image
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.use-case.image)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (cocoa.entity.fs.image:image-id image)
        :path (cocoa.entity.fs.image:image-path image)))

(defstruct add-images image-factory image-dao)
(export 'make-add-images)

@export
(defun add-images (add-images paths)
  "The use case of adding images"
  (let ((images (cocoa.entity.fs.image:make-by-paths
                 (add-images-image-factory add-images)
                 paths)))
    (cocoa.entity.fs.image:save (add-images-image-dao add-images)
                                images)
    (mapcar #'image->dto images)))

@export
(defun get-path (image-dao id)
  "The use case of getting the path of an image"
  (when-let ((image (car (cocoa.entity.fs.image:list-by-ids
                          image-dao
                          (list id)))))
    (cocoa.entity.fs.image:image-path image)))
