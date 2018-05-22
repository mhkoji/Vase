(defpackage :cocoa.use-case.folder.add
  (:use :cl
        :cocoa.entity.folder
        :cocoa.util.stream))
(in-package :cocoa.use-case.folder.add)
(cl-annot:enable-annot-syntax)

@export
(defun add-by-source-stream (source-stream &key folder-repository)
  (save-folders/sources folder-repository (stream-to-list source-stream)))


(defun image-id (image)
  (getf image :id))

(defun make-thumbnail (path &key image-factory image-repository)
  (let ((image (car (cocoa.use-case.image:add-images (list path)
                     :image-factory image-factory
                     :image-repository image-repository))))
    (cocoa.use-case.folder.thumbnail:make-of-image (image-id image))))


(defun image->content (image)
  (cocoa.use-case.folder.content:make-of-image (image-id image)))

(defun make-image-contents (paths &key image-factory image-repository)
  (mapcar #'image->content (cocoa.use-case.image:add-images paths
                            :image-factory image-factory
                            :image-repository image-repository)))


;;; A representation of a directory in the local file system
(defstruct dir path file-paths)
(export 'dir)
(export 'make-dir)

@export
(defun dir->source-converter (&key (sort-file-paths #'identity)
                                   make-thumbnail-file
                                   path->folder-id
                                   image-factory
                                   image-repository)
  (lambda (dir)
    (let ((path (dir-path dir))
          (file-paths (funcall sort-file-paths (dir-file-paths dir))))
      (make-source
       :folder-id (funcall path->folder-id path)
       :name path
       :modified-at (file-write-date path)
       :contents (make-image-contents file-paths
                                      :image-factory image-factory
                                      :image-repository image-repository)
       :thumbnail (make-thumbnail (funcall make-thumbnail-file
                                           (car file-paths))
                                  :image-factory image-factory
                                  :image-repository image-repository)))))
