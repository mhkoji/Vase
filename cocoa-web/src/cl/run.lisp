(defpackage :cocoa.web
  (:use :cl
        :cocoa.ext.context
        :cocoa.web.bind)
  (:import-from :cl-arrows :->))
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
      (cocoa.ext.fs.thumbnail:ensure-thumbnail-exists thumbnail-file
                                                        source-file)
      thumbnail-file)))

@export
(defun add-folders (root-dir
                    &key (context (load-context))
                         (sort-file-paths #'identity)
                         (initialize-data-p t))
  (with-dao (dao context)
    (when initialize-data-p
      (initialize dao))
    (-> (cocoa.use-case.folder.add-bulk-by-dirs:prepare
         :folder-repository (cocoa.folder:folder-repository dao)
         :path->folder-id (context-digest-fn context)
         :sort-file-paths sort-file-paths
         :make-thumbnail-file (make-thumbnail-file-factory
                               (context-thumbnail-root context))
         :use-case/image/add (cocoa.use-case.image.add:prepare
                              (cocoa.fs.image:image-repository dao)
                              (context-digest-fn context)))
        (cocoa.use-case.folder.add-bulk-by-dirs:exec
         (cocoa.util.stream:stream-to-list
          (cocoa.util.stream:stream-map
           (lambda (args)
             (destructuring-bind (&key path file-paths) args
               (cocoa.use-case.folder.add-bulk-by-dirs:make-dir
                :path path :file-paths file-paths)))
           (cocoa.ext.fs.retrieve:retrieve root-dir)))))))
