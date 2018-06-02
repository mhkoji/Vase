(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

;;; The representation of the source of a folder
(defstruct source name modified-at thumbnail contents)

@export
(defun add-by-source-stream (source-stream &key folder-dao name->folder-id)
  (let* ((source-list
          (cocoa.util.stream:stream-to-list source-stream))
         (folder-ids
          (mapcar (lambda (source)
                    (funcall name->folder-id (source-name source)))
                  source-list)))
    (let ((configs (mapcar (lambda (folder-id source)
                             (cocoa.entity.folder:make-folder-config
                              :id folder-id
                              :name (source-name source)
                              :thumbnail (source-thumbnail source)
                              :modified-at (source-modified-at source)))
                           folder-ids source-list)))
      (setq folder-dao (cocoa.entity.folder:save folder-dao configs)))
    (loop for folder in (cocoa.entity.folder:list-by-ids folder-dao
                         (cocoa.entity.folder:make-list-spec)
                         folder-ids)
          for source in source-list
       do (let ((diff (cocoa.entity.folder:append-contents
                       (source-contents source))))
            (cocoa.entity.folder:update! folder diff))))
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
(defun dir->source-converter (&key (sort-file-paths #'identity)
                                   make-thumbnail-file
                                   image-dao
                                   image-factory)
  (lambda (dir)
    (let ((path (dir-path dir))
          (file-paths (funcall sort-file-paths (dir-file-paths dir))))
      (make-source
       :name path
       :modified-at (file-write-date path)
       :thumbnail (let ((thumbnail-file
                         (funcall make-thumbnail-file (car file-paths))))
                    (make-thumbnail thumbnail-file
                                    :image-dao image-dao
                                    :image-factory image-factory))
       :contents (make-image-contents file-paths
                                      :image-dao image-dao
                                      :image-factory image-factory)))))
