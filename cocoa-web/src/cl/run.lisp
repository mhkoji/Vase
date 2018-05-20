(defpackage :cocoa.web
  (:use :cl
        :cocoa.infra.context
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


(defun make-thumbnail-factory (thumbnail-root)
  (lambda (source-file)
    (log:debug "Creating thumbnail for: ~A" source-file)
    (let ((thumbnail-file
           (format nil
                   "~Athumbnail$~A"
                   thumbnail-root
                   (cl-ppcre:regex-replace-all "/" source-file "$"))))
      (cocoa.infra.file.thumbnail:ensure-thumbnail-exists thumbnail-file
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
    (let ((dir-stream
           (cocoa.util.stream:stream-map
            (lambda (dir-source)
              (apply #'cocoa.use-case.folder.add:make-dir dir-source))
            (cocoa.infra.file.retrieve:retrieve root-dir))))
      (cocoa.use-case.folder.add:add-by-local-directories
       dir-stream
       :sort-file-paths sort-file-paths
       :make-thumbnail-file (make-thumbnail-factory
                             (context-thumbnail-root context))
       :path->folder-id (context-digest-fn context)
       :folder-repository dao
       :image-factory (context-digest-fn context)
       :image-repository dao))))
