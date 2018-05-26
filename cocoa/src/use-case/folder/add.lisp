(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

@export
(defun add-by-props-stream (props-stream &key name->folder-id
                                              folder-repository)
  (let ((props-list (cocoa.util.stream:stream-to-list props-stream)))
    (cocoa.entity.folder:add-folders/sources folder-repository
     (mapcar (lambda (props)
               (let ((name (getf props :name)))
                 (make-source :folder-id   (funcall name->folder-id name)
                              :name        name
                              :thumbnail   (getf props :thumbnail)
                              :modified-at (getf props :modified-at))))
             props-list))
    (dolist (props props-list)
      (cocoa.entity.folder.content:add-by-folder-id folder-repository
       (funcall name->folder-id (getf props :name))
       (getf props :contents))))
  (values))


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
(defun dir->props-converter (&key (sort-file-paths #'identity)
                                  make-thumbnail-file
                                  image-factory
                                  image-repository)
  (lambda (dir)
    (let ((path (dir-path dir))
          (file-paths (funcall sort-file-paths (dir-file-paths dir))))
      (list
       :name path
       :modified-at (file-write-date path)
       :thumbnail (make-thumbnail (funcall make-thumbnail-file
                                           (car file-paths))
                   :image-factory image-factory
                   :image-repository image-repository)
       :contents (make-image-contents file-paths
                  :image-factory image-factory
                  :image-repository image-repository)))))
