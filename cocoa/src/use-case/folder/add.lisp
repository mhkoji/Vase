(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

;;; The representation of the source of a folder
(defstruct source name modified-at thumbnail contents)
(export 'make-source)

@export
(defun add-with-contents (sources &key folder-dao name->folder-id)
  (let ((folder-ids (mapcar (alexandria:compose name->folder-id
                                                #'source-name)
                            sources)))
    (-> folder-dao
        (cocoa.entity.folder:handle!
         (cocoa.entity.folder:bulk :add
          (mapcar (lambda (folder-id source)
                    (cocoa.entity.folder:add
                     :id folder-id
                     :name (source-name source)
                     :thumbnail (source-thumbnail source)
                     :modified-at (source-modified-at source)))
                  folder-ids sources)))
        (cocoa.entity.folder:handle!
         (cocoa.entity.folder:bulk :append-contents
          (mapcar (lambda (folder-id source)
                    (cocoa.entity.folder:append-contents
                     folder-id
                     (source-contents source)))
                  folder-ids sources))))))

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
