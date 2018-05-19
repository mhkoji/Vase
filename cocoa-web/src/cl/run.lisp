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


(defun thumbnail-factory (thumbnail-root)
  (lambda (source-file)
    (log:info "Creating thumbnail for: ~A" source-file)
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
                         (sort-files #'identity)
                         (initialize-data-p t))
  (with-dao (dao context)
    (when initialize-data-p
      (initialize dao))
    (let ((stream
           (cocoa.infra.file.retrieve:retrieve root-dir
            :sort-files sort-files
            :make-thumbnail (thumbnail-factory
                             (context-thumbnail-root context))))
          (digest-fn
           (context-digest-fn context)))
      (cocoa.use-case.folder.create:execute stream
       :image-factory digest-fn :image-repository dao
       :path->folder-id digest-fn :folder-repository dao))))
