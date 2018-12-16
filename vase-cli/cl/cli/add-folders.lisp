(defpackage :vase.cli.add-folders
  (:use :cl)
  (:export :execute)
  (:import-from :vase.cli.stream
                :stream-to-list))
(in-package :vase.cli.add-folders)

(defun add-images (id-generator image-repository paths)
  (let ((images (vase.image:bulk-create id-generator paths)))
    (vase.image:bulk-save image-repository images)
    images))

(defun make-source-from-dir (dir &key id-generator
                                      image-repository
                                      thumbnail-file-fn)
  (let ((thumbnail-source-path (car (vase.cli.fs:dir-file-paths dir))))
    (let ((thumbnail-path (funcall thumbnail-file-fn
                                   thumbnail-source-path)))
      (let ((image (car (add-images id-generator
                                    image-repository
                                    (list thumbnail-path)))))
        (vase.folder:make-source
         :name (vase.cli.fs:dir-path dir)
         :thumbnail (vase.folder.thumbnail:from-image image)
         :modified-at (vase.cli.fs:dir-modified-at dir))))))

(defun make-appending-for (folder dir &key id-generator image-repository)
  (let ((contents (mapcar #'vase.folder.content:from-image
                          (add-images id-generator
                                      image-repository
                                      (vase.cli.fs:dir-file-paths dir)))))
    (vase.folder.content:make-appending :folder folder :contents contents)))

(defun execute (conf root-dir &key sort-paths-fn thumbnail-file-fn)
  (vase.context:with-context (context) conf
    (with-accessors ((db vase.context:db)
                     (id-gen vase.context:id-gen)
                     (image-repos vase.context:image-repos)) context
      (let ((dirs (stream-to-list (vase.cli.fs:retrieve root-dir
                                                        sort-paths-fn))))
        (let ((sources (mapcar (lambda (d)
                                 (make-source-from-dir d
                                  :id-generator id-gen
                                  :image-repository image-repos
                                  :thumbnail-file-fn thumbnail-file-fn))
                               dirs)))
          (let ((folders (vase.folder:bulk-create id-gen sources)))
            ;; Delete existing folders if any
            (vase.folder:bulk-delete db (mapcar #'vase.folder:folder-id
                                                folders))
            ;; Save folders
            (vase.folder:bulk-save db folders)
            (let ((appendings (mapcar (lambda (folder dir)
                                        (make-appending-for folder dir
                                         :id-generator id-gen
                                         :image-repository image-repos))
                                      folders dirs)))
              ;; Save folder contents
              (vase.folder.content:bulk-append db appendings)))))))
  (values))
