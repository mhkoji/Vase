(defpackage :cocoa.web-simple
  (:use :cl :cocoa.di.context)
  (:import-from :cl-arrows :-> :->>))
(in-package :cocoa.web-simple)
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
             (cocoa.web-simple.controller:bind-app!
              :context context)
             (cocoa.web-simple.controller:bind-resources!
              (namestring *default-pathname-defaults*)))
         :port port)))
