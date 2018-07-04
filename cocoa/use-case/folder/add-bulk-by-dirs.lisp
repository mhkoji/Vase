(defpackage :cocoa.use-case.folder.add-bulk-by-dirs
  (:use :cl)
  (:import-from :cl-arrows :-> :->>))
(in-package :cocoa.use-case.folder.add-bulk-by-dirs)
(cl-annot:enable-annot-syntax)

;;; A representation of a directory in the local file system
(defstruct dir path file-paths modified-at)
(export 'make-dir)

(defstruct image id)
(export 'make-image)

(defun dir-thumbnail (dir &key make-thumbnail-file add-images-by-paths)
  (let ((thumbnail-file
         (funcall make-thumbnail-file (car (dir-file-paths dir)))))
    (cocoa.use-case.folder.thumbnail:make-of-image
     (image-id
      (car (funcall add-images-by-paths (list thumbnail-file)))))))

(defun dir-folder-config (dir folder-id
                          &key make-thumbnail-file
                               add-images-by-paths)
  (let ((path (dir-path dir)))
    (cocoa.folder:make-folder-config
     :id folder-id
     :name path
     :thumbnail (dir-thumbnail dir
                 :make-thumbnail-file make-thumbnail-file
                 :add-images-by-paths add-images-by-paths)
     :modified-at (dir-modified-at dir))))

(defun dir-appending (dir folder-id &key add-images-by-paths)
  (cocoa.folder:make-appending
   :folder-id folder-id
   :contents (mapcar (alexandria:compose
                      #'cocoa.use-case.folder.content:make-of-image
                      #'image-id)
                     (funcall add-images-by-paths (dir-file-paths dir)))))

@export
(defun prepare (&key folder-repository
                     path->folder-id
                     make-thumbnail-file
                     add-images-by-paths)
  (labels ((folder-config (dir folder-id)
             (dir-folder-config dir folder-id
              :make-thumbnail-file make-thumbnail-file
              :add-images-by-paths add-images-by-paths))
           (appending (dir folder-id)
             (dir-appending dir folder-id
              :add-images-by-paths add-images-by-paths)))
    (lambda (dirs)
      (let ((folder-ids (mapcar (alexandria:compose
                                 path->folder-id #'dir-path)
                                dirs)))
        (-> folder-repository
            (cocoa.folder:save-folders
             (mapcar #'folder-config dirs folder-ids))
            (cocoa.folder:update-contents
             (cocoa.folder:make-appending-bulk
              :appendings (mapcar #'appending dirs folder-ids))))))))


@export
(defun exec (prepare dirs)
  (funcall prepare dirs))
