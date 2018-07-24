(defpackage :cocoa.web
  (:use :cl
        :cocoa.di.context
        :cocoa.web.bind)
  (:import-from :cl-arrows :-> :->>))
(in-package :cocoa.web)
(cl-annot:enable-annot-syntax)

(defvar *handler* nil)

@export
(defun run (&key (port 18888)
                 (context (load-context)))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (-> (make-instance 'ningle:<app>)
             (bind-resources! (namestring *default-pathname-defaults*))
             (bind-api! :context context)
             (bind-html!))
         :port port)))

(defun make-thumbnail-file-factory (thumbnail-root)
  (lambda (source-file)
    (log:debug "Creating thumbnail for: ~A" source-file)
    (let ((thumbnail-file
           (format nil
                   "~Athumbnail$~A"
                   thumbnail-root
                   (cl-ppcre:regex-replace-all "/" source-file "$"))))
      (cocoa.util.fs.thumbnail:ensure-thumbnail-exists thumbnail-file
                                                        source-file)
      thumbnail-file)))

@export
(defun add-folders (root-dir
                    &key (context (load-context))
                         (sort-file-paths #'identity)
                         (initialize-data-p t))
  (with-db (db context)
    (when initialize-data-p
      (initialize db))
    (cocoa.folder:add-bulk
     (cocoa.util.stream:stream-to-list
      (cocoa.util.stream:stream-map
       (lambda (args)
         (destructuring-bind (&key path file-paths) args
           (cocoa.folder:make-dir
            :path path
            ;; Assume that all the files in each dir are an image.
            :image-paths (funcall sort-file-paths file-paths)
            :modified-at (file-write-date path))))
       (cocoa.util.fs.retrieve:retrieve root-dir)))
     :db db
     :id-generator (context-id-generator context)
     :make-thumbnail-file
     (make-thumbnail-file-factory (context-thumbnail-root context))))
  (values))
