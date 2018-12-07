(defpackage :vase.cli
  (:use :cl)
  (:export :add-folders))
(in-package :vase.cli)

(defun add-folders (root-dir
                    &key (conf (vase.contexts.configure:load-configure))
                         (sort-paths-fn #'identity)
                         (initialize-data-p nil))
  (when initialize-data-p
    (vase.contexts.configure:with-db (db conf)
      (vase.contexts.configure:initialize-db db)))
  (labels ((make-thumbnail-path (source-file)
             (format nil "~Athumbnail$~A"
                     (vase.contexts.configure:configure-thumbnail-root
                      conf)
                     (cl-ppcre:regex-replace-all "/" source-file "$"))))
    (vase.cli.add-folders:execute conf root-dir
     :sort-paths-fn sort-paths-fn
     :thumbnail-file-fn
     (lambda (source-path)
       (log:debug "Creating thumbnail for: ~A" source-path)
       (let ((thumbnail-path (make-thumbnail-path source-path)))
         (vase.cli.fs:ensure-thumbnail-exists thumbnail-path source-path)
         thumbnail-path)))))
