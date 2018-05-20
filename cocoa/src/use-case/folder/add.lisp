(defpackage :cocoa.use-case.folder.add
  (:use :cl
        :cocoa.entity.folder
        :cocoa.util.stream))
(in-package :cocoa.use-case.folder.add)
(cl-annot:enable-annot-syntax)

@export
(defun add-by-source-stream (source-plist-stream
                             &key name->folder-id folder-repository)
  (let ((sources nil))
    (do-stream (source-plist source-plist-stream)
      (destructuring-bind (&key name modified-at thumbnail contents)
          source-plist
        (push (make-source :folder-id (funcall name->folder-id name)
                           :name name
                           :contents contents
                           :thumbnail thumbnail
                           :modified-at modified-at)
              sources)))
    (save-folders/sources folder-repository sources)))


(defun image-id (image)
  (getf image :id))

(defun make-thumbnail (path &key image-factory image-repository)
  (let ((image (car (cocoa.use-case.image:add-images (list path)
                     :image-factory image-factory
                     :image-repository image-repository))))
    (cocoa.entity.folder.thumbnail:make-image-thumbnail (image-id image))))

(defun make-image-contents (paths &key image-factory image-repository)
  (mapcar (lambda (image)
            (cocoa.entity.folder.content:make-image-content
             (image-id image)))
          (cocoa.use-case.image:add-images paths
           :image-factory image-factory
           :image-repository image-repository)))

;;; A representation of a directory in the local file system
(defstruct dir path file-paths)
(export 'dir)
(export 'make-dir)

@export
(defun add-by-local-directories (dir-stream
                                 &key (sort-file-paths #'identity)
                                      make-thumbnail-file
                                      image-factory
                                      image-repository
                                      name->folder-id
                                      folder-repository)
  (labels ((dir->source-plist (dir)
             (let ((path (dir-path dir))
                   (file-paths (funcall sort-file-paths
                                        (dir-file-paths dir))))
               (list :name path
                     :modified-at (file-write-date path)
                     :contents (make-image-contents
                                file-paths
                                :image-factory image-factory
                                :image-repository image-repository)
                     :thumbnail (make-thumbnail
                                 (funcall make-thumbnail-file
                                          (car file-paths))
                                 :image-factory image-factory
                                 :image-repository image-repository)))))
    (add-by-source-stream (stream-map #'dir->source-plist dir-stream)
                          :name->folder-id name->folder-id
                          :folder-repository folder-repository)))
