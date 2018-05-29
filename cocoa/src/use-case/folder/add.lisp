(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

@export
(defun add-by-props-stream (props-stream &key folder-dao name->folder-id)
  (let* ((props-list (cocoa.util.stream:stream-to-list props-stream))
         (folder-ids (mapcar (lambda (props)
                               (funcall name->folder-id
                                        (getf props :name)))
                             props-list)))
    (cocoa.entity.folder:save folder-dao
     (mapcar (lambda (folder-id props)
               (cocoa.entity.folder:make-source
                :folder-id   folder-id
                :name        (getf props :name)
                :thumbnail   (getf props :thumbnail)
                :modified-at (getf props :modified-at)))
             folder-ids props-list))
    (let ((folders (cocoa.entity.folder:list-by-ids folder-dao
                    (cocoa.entity.folder:make-list-spec)
                    folder-ids))
          (contents-list (mapcar (lambda (props) (getf props :contents))
                                 props-list)))
      (loop for folder in folders
            for contents in contents-list
            do (progn (cocoa.entity.folder:update! folder
                       (cocoa.entity.folder:append-contents contents))))))
  (values))

(defun image-id (image)
  (getf image :id))

(defun make-thumbnail (path &key image-dao image-factory)
  (let ((image (car (cocoa.use-case.image:add-images (list path)
                     :image-dao image-dao
                     :image-factory image-factory))))
    (cocoa.use-case.folder.thumbnail:make-of-image (image-id image))))


(defun image->content (image)
  (cocoa.use-case.folder.content:make-of-image (image-id image)))

(defun make-image-contents (paths &key image-dao image-factory)
  (mapcar #'image->content (cocoa.use-case.image:add-images paths
                            :image-dao image-dao
                            :image-factory image-factory)))

;;; A representation of a directory in the local file system
(defstruct dir path file-paths)
(export 'dir)
(export 'make-dir)

@export
(defun dir->props-converter (&key (sort-file-paths #'identity)
                                  make-thumbnail-file
                                  image-dao
                                  image-factory)
  (lambda (dir)
    (let ((path (dir-path dir))
          (file-paths (funcall sort-file-paths (dir-file-paths dir))))
      (list :name path
            :modified-at (file-write-date path)
            :thumbnail
            (let ((thumbnail-file
                   (funcall make-thumbnail-file (car file-paths))))
              (make-thumbnail thumbnail-file
                              :image-dao image-dao
                              :image-factory image-factory))
            :contents
            (make-image-contents file-paths
                                 :image-dao image-dao
                                 :image-factory image-factory)))))
