(defpackage :vase.app.cli.add-folders
  (:use :cl)
  (:export :execute)
  (:import-from :vase.app.cli.stream
                :stream-to-list))
(in-package :vase.app.cli.add-folders)

(defun add-images (id-generator image-repository paths)
  (let ((images (vase.image:bulk-create id-generator paths)))
    (vase.image:bulk-save image-repository images)
    images))

(defun make-source-from-dir (dir &key id-generator
                                      image-repository
                                      thumbnail-file-fn)
  (let ((thumbnail-source-path (car (vase.app.cli.fs:dir-file-paths dir))))
    (let ((thumbnail-path (funcall thumbnail-file-fn
                                   thumbnail-source-path)))
      (let ((image (car (add-images id-generator
                                    image-repository
                                    (list thumbnail-path)))))
        (vase.folder:make-source
         :name (vase.app.cli.fs:dir-path dir)
         :thumbnail image
         :modified-at (vase.app.cli.fs:dir-modified-at dir))))))

(defun make-appending-for (folder dir &key id-generator image-repository)
  (let ((contents (add-images id-generator
                              image-repository
                              (vase.app.cli.fs:dir-file-paths dir))))
    (vase.folder.content:make-appending :folder folder :contents contents)))

(defun execute (root-dir &key db
                              id-generator
                              image-repository
                              sort-paths-fn
                              thumbnail-file-fn)
  (let ((dirs (stream-to-list (vase.app.cli.fs:retrieve root-dir
                                                    sort-paths-fn))))
    (let ((sources (mapcar (lambda (d)
                             (make-source-from-dir d
                              :id-generator id-generator
                              :image-repository image-repository
                              :thumbnail-file-fn thumbnail-file-fn))
                           dirs)))
      (let ((folders (vase.folder:bulk-create id-generator sources)))
        ;; Delete existing folders if any
        (vase.folder:bulk-delete db (mapcar #'vase.folder:folder-id folders))
        ;; Save folders
        (vase.folder:bulk-save db folders)
        (let ((appendings (mapcar (lambda (folder dir)
                                    (make-appending-for folder dir
                                     :id-generator id-generator
                                     :image-repository image-repository))
                                  folders dirs)))
          ;; Save folder contents
          (vase.folder.content:bulk-append db appendings)))))
  (values))
