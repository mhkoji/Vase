(defpackage :vase.cli.add-folders
  (:use :cl :vase)
  (:import-from :vase.cli.add-folders.fs.thumbnail
                :ensure-thumbnail-exists)
  (:import-from :vase.cli.add-folders.fs.retrieve
                :retrieve)
  (:import-from :vase.cli.add-folders.stream
                :stream-to-list :stream-map))
(in-package :vase.cli.add-folders)
(cl-annot:enable-annot-syntax)

(defun get-make-thumbnail-file-fn (thumbnail-root)
  (lambda (source-file)
    (log:debug "Creating thumbnail for: ~A" source-file)
    (let ((thumbnail-file
           (format nil
                   "~Athumbnail$~A"
                   thumbnail-root
                   (cl-ppcre:regex-replace-all "/" source-file "$"))))
      (ensure-thumbnail-exists thumbnail-file source-file)
      thumbnail-file)))

(defun retrieve-dirs (root-dir sort-file-paths)
  (stream-to-list
   (stream-map (lambda (args)
                 (destructuring-bind (&key path file-paths) args
                   (vase.app.folder:make-dir
                    :path path
                    ;; Assume that all the files in each dir are an image.
                    :image-paths (funcall sort-file-paths file-paths)
                    :modified-at (file-write-date path))))
               (retrieve root-dir))))

@export
(defun execute (root-dir
                &key (context (load-context))
                     (sort-file-paths #'identity)
                     (initialize-data-p t))
  (with-db (db context)
    (when initialize-data-p
      (initialize db))
    (let ((dirs (retrieve-dirs root-dir sort-file-paths)))
      (vase.app.folder:add-bulk db dirs
       :id-generator
       (context-id-generator context)
       :make-thumbnail-file-fn
       (get-make-thumbnail-file-fn (context-thumbnail-root context)))))
  (values))
