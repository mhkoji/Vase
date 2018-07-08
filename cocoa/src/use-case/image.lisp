(defpackage :cocoa.use-case.image
  (:use :cl)
  (:import-from :alexandria
                :when-let))
(in-package :cocoa.use-case.image)
(cl-annot:enable-annot-syntax)

(defun image->dto (image)
  (list :id (cocoa.fs.image:image-id image)
        :path (cocoa.fs.image:image-path image)))

@export
(defun add-images (paths &key image-repository path->image-id)
  "The use case of adding images"
  (let ((image-ids (mapcar path->image-id paths)))
    (let ((images (mapcar #'cocoa.fs.image:make-image image-ids paths)))
      (cocoa.fs.image:save-images image-repository images)
      (mapcar #'image->dto images))))

@export
(defun get-path (id &key image-repository)
  "The use case of "
  (when-let ((image (car (cocoa.fs.image:load-images-by-ids
                          image-repository
                          (list id)))))
    (cocoa.fs.image:image-path image)))
