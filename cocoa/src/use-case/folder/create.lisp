(defpackage :cocoa.use-case.folder.create
  (:use :cl
        :cocoa.util.stream
        :cocoa.entity.folder
        :cocoa.use-case.folder.inject))
(in-package :cocoa.use-case.folder.create)
(cl-annot:enable-annot-syntax)

@export
(defun execute (folder-files-stream
                &key path->folder-id
                     image-factory
                     image-repository
                     folder-repository)
  (let ((sources nil))
    (do-stream (folder-files folder-files-stream)
      (destructuring-bind (&key path files thumbnail-file) folder-files
        (let ((images (cocoa.entity.image:make-images/paths
                       image-factory
                       (cons thumbnail-file files))))
          (cocoa.entity.image:save-images image-repository images)
          (push (make-source
                 :name path
                 :folder-id (funcall path->folder-id path)
                 :thumbnail (image->thumbnail (car images))
                 :contents (mapcar #'image->content (cdr images))
                 :modified-at (file-write-date path))
                sources))))
    (save-folders/sources folder-repository sources)))
