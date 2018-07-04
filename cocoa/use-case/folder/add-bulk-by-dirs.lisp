(defpackage :cocoa.use-case.folder.add-bulk-by-dirs
  (:use :cl)
  (:import-from :cl-arrows :-> :->>))
(in-package :cocoa.use-case.folder.add-bulk-by-dirs)
(cl-annot:enable-annot-syntax)

;;; A representation of a directory in the local file system
(defstruct dir path file-paths)
(export 'make-dir)

(defun dir-folder-config (dir folder-id
                          &key make-thumbnail-file
                               sort-file-paths
                               use-case/image/add)
  (let ((path (dir-path dir))
        (file-paths (funcall sort-file-paths (dir-file-paths dir))))
    (cocoa.folder:make-folder-config
     :id folder-id
     :name path
     :thumbnail (let ((thumbnail-file
                       (funcall make-thumbnail-file (car file-paths))))
                  (let ((thumbnail-image
                         (car (cocoa.use-case.image.add:exec
                               use-case/image/add
                               (list thumbnail-file)))))
                    (cocoa.use-case.folder.thumbnail:make-of-image
                     (getf thumbnail-image :id))))
     :modified-at (file-write-date path))))

(defun dir-appending (dir folder-id &key use-case/image/add)
  (labels ((image-id (image)
             (getf image :id)))
    (cocoa.folder:make-appending
     :folder-id folder-id
     :contents (mapcar (alexandria:compose
                        #'cocoa.use-case.folder.content:make-of-image
                        #'image-id)
                       (cocoa.use-case.image.add:exec use-case/image/add
                        (dir-file-paths dir))))))

@export
(defun prepare (&key folder-repository
                     path->folder-id
                     sort-file-paths
                     make-thumbnail-file
                     use-case/image/add)
  (labels ((make-folder-config (dir folder-id)
             (dir-folder-config dir folder-id
              :make-thumbnail-file make-thumbnail-file
              :sort-file-paths sort-file-paths
              :use-case/image/add use-case/image/add))
           (make-appending (dir folder-id)
             (dir-appending dir folder-id
              :use-case/image/add use-case/image/add)))
    (lambda (dirs)
      (let ((folder-ids (mapcar (alexandria:compose
                                 path->folder-id #'dir-path)
                                dirs)))
        (-> folder-repository
            (cocoa.folder:save-folders
             (mapcar #'make-folder-config dirs folder-ids))
            (cocoa.folder:update-contents
             (cocoa.folder:make-appending-bulk
              :appendings (mapcar #'make-appending dirs folder-ids))))))))


@export
(defun exec (prepare dirs)
  (funcall prepare dirs))
