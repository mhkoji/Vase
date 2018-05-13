(defpackage :cocoa.controller.cli.setup-folder
  (:use :cl :cocoa.controller.context))
(in-package :cocoa.controller.cli.setup-folder)
(cl-annot:enable-annot-syntax)

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
(defun setup (root-dir
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
