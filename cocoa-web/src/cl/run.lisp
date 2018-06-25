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



(defun make-thumbnail-file-factory (thumbnail-root)
  (lambda (source-file)
    (log:debug "Creating thumbnail for: ~A" source-file)
    (let ((thumbnail-file
           (format nil
                   "~Athumbnail$~A"
                   thumbnail-root
                   (cl-ppcre:regex-replace-all "/" source-file "$"))))
      (cocoa.infra.fs.thumbnail:ensure-thumbnail-exists thumbnail-file
                                                        source-file)
      thumbnail-file)))

(defun make-thumbnail (add-images path)
  (-> (car (cocoa.use-case.image.add:call add-images (list path)))
      (getf :id)
      (cocoa.use-case.folder.thumbnail:make-of-image)))

(defun make-image-contents (add-images paths)
  (labels ((image->content (image)
             (-> image
                 (getf :id)
                 (cocoa.use-case.folder.content:make-of-image))))
    (mapcar #'image->content
            (cocoa.use-case.image.add:call add-images paths))))

;;; A representation of a directory in the local file system
(defstruct dir path file-paths)

(defun make-dir->source-converter (sort-file-paths
                                   make-thumbnail-file
                                   add-images)
  (lambda (dir)
    (let ((path (dir-path dir))
          (file-paths (funcall sort-file-paths (dir-file-paths dir))))
      (cocoa.use-case.folder.add-bulk:make-source
       :name path
       :modified-at (file-write-date path)
       :thumbnail (let ((thumbnail-file
                         (funcall make-thumbnail-file (car file-paths))))
                    (make-thumbnail add-images thumbnail-file))
       :contents (make-image-contents add-images file-paths)))))

@export
(defun add-folders (root-dir
                    &key (context (load-context))
                         (sort-file-paths #'identity)
                         (initialize-data-p t))
  (with-dao (dao context)
    (when initialize-data-p
      (initialize dao))
    (let ((dir-stream (cocoa.util.stream:stream-map
                       (lambda (dir-source) (apply #'make-dir dir-source))
                       (cocoa.infra.fs.retrieve:retrieve root-dir))))
      (cocoa.use-case.folder.add-bulk:call
       (cocoa.use-case.folder.add-bulk:make-add-bulk
        :folder-dao dao
        :name->folder-id (context-digest-fn context))
       (mapcar (make-dir->source-converter
                sort-file-paths
                (make-thumbnail-file-factory
                 (context-thumbnail-root context))
                (cocoa.use-case.image.add:make-add-images
                 :image-dao dao
                 :image-factory (context-digest-fn context)))
               (cocoa.util.stream:stream-to-list dir-stream))))))

