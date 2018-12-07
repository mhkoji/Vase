(defpackage :vase.cli.add-folders
  (:use :cl)
  (:export :execute)
  (:import-from :vase.cli.stream
                :stream-to-list))
(in-package :vase.cli.add-folders)

(defun add-images (image-factory image-repository paths)
  (let ((images (vase.image:bulk-create image-factory paths)))
    (vase.image:bulk-save image-repository images)
    images))

(defun make-source-from-dir (dir &key image-repository
                                      image-factory
                                   thumbnail-file-fn)
  (let ((thumbnail-source-path (car (vase.cli.fs:dir-file-paths dir))))
    (let ((thumbnail-path (funcall thumbnail-file-fn
                                   thumbnail-source-path)))
      (let ((image (car (add-images image-factory
                                    image-repository
                                    (list thumbnail-path)))))
        (vase.folder:make-source
         :name (vase.cli.fs:dir-path dir)
         :thumbnail (vase.folder.thumbnail:from-image image)
         :modified-at (vase.cli.fs:dir-modified-at dir))))))

(defun execute (conf root-dir &key sort-paths-fn thumbnail-file-fn)
  (let ((dirs (stream-to-list (vase.cli.fs:retrieve root-dir
                                                    sort-paths-fn))))
    (let ((sources
           (vase.contexts:with-image ((image-repos image-factory) conf)
             (mapcar (lambda (d)
                       (make-source-from-dir d
                        :image-factory image-factory
                        :image-repository image-repos
                        :thumbnail-file-fn thumbnail-file-fn))
                     dirs))))
    ;; Save folders
      (let ((folder-ids
             (vase.contexts:with-folder ((folder-repos folder-factory) conf)
               (vase.folder:bulk-save folder-repos
                                      folder-factory
                                      sources))))
        ;; Save folder contents
        (let ((appendings
               (vase.contexts:with-image ((image-repos image-factory) conf)
                 (mapcar (lambda (dir folder-id)
                           (vase.folder.content.db:make-appending
                            :folder-id folder-id
                            :contents
                            (mapcar #'vase.folder.content.entities:from-image
                                    (add-images image-factory
                                                image-repos
                                                (vase.cli.fs:dir-file-paths
                                                 dir)))))
                         dirs folder-ids))))
          (vase.contexts:with-folder ((folder-repos folder-factory) conf)
            (declare (ignore folder-factory))
            (vase.folder:bulk-append-contents folder-repos appendings))))))
  (values))
