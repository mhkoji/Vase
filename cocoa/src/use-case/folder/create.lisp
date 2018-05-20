(defpackage :cocoa.use-case.folder.create
  (:use :cl
        :cocoa.util.stream
        :cocoa.entity.folder
        :cocoa.use-case.folder.inject))
(in-package :cocoa.use-case.folder.create)
(cl-annot:enable-annot-syntax)

@export
(defun execute (folder-files-stream
                &key image-factory
                     image-repository
                     path->folder-id
                     folder-repository)
  (let ((sources nil))
    (do-stream (folder-files folder-files-stream)
      (destructuring-bind (&key path files thumbnail-file) folder-files
        (push (make-source
               :name path
               :folder-id (funcall path->folder-id path)
               :modified-at (file-write-date path)
               :thumbnail (make-thumbnail thumbnail-file
                           :image-factory image-factory
                           :image-repository image-repository)
               :contents (make-image-contents files
                          :image-factory image-factory
                          :image-repository image-repository))
              sources)))
    (save-folders/sources folder-repository sources)))
