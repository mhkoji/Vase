(defpackage :cocoa.image
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.image)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (cocoa.entity.fs.image:image-id image)
        :path (cocoa.entity.fs.image:image-path image)))

@export
(defun add-images (paths &key image-repository path->image-id)
  "The use case of adding images"
  (let ((image-ids (mapcar path->image-id paths)))
    (let ((images (mapcar #'cocoa.entity.fs.image:make-image
                          image-ids
                          paths)))
      (cocoa.entity.fs.image:save-images image-repository images)
      (mapcar #'image->dto images))))

@export
(defun get-path (id &key image-repository)
  "The use case of "
  (when-let ((image (car (cocoa.entity.fs.image:load-images-by-ids
                          image-repository
                          (list id)))))
    (cocoa.entity.fs.image:image-path image)))
