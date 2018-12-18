(defpackage :vase.app.cli
  (:use :cl :vase.app.container)
  (:export :add-folders))
(in-package :vase.app.cli)

(defun add-folders (root-dir
                    &key (conf (vase.app.container:load-configure))
                         (sort-paths-fn #'identity)
                         (initialize-data-p nil))
  (when initialize-data-p
    (with-container (c conf)
      (vase.db:initialize (container-db c))))
  (labels ((make-thumbnail-path (source-path)
             (format nil "~Athumbnail$~A"
                     (configure-thumbnail-root conf)
                     (cl-ppcre:regex-replace-all "/" source-path "$")))
           (make-thumbnail-file (source-path)
             (log:debug "Creating thumbnail for: ~A" source-path)
             (let ((thumbnail-path (make-thumbnail-path source-path)))
               (vase.app.cli.fs:ensure-thumbnail-exists thumbnail-path
                                                        source-path)
               thumbnail-path)))
    (with-container (c conf)
      (vase.app.cli.add-folders:execute root-dir
       :db                (container-db c)
       :id-generator      (container-id-generator c)
       :image-repository  (container-image-repository c)
       :sort-paths-fn     sort-paths-fn
       :thumbnail-file-fn #'make-thumbnail-file))))
