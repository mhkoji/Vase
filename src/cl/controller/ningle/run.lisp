(defpackage :cocoa.controller.ningle
  (:use :cl
        :cocoa.controller.context
        :cocoa.controller.ningle.bind)
  (:import-from :cl-arrows :->))
(in-package :cocoa.controller.ningle)
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
