(defpackage :vase.spa
  (:use :cl :vase.spa.bind)
  (:import-from :cl-arrows :-> :->>))
(in-package :vase.spa)
(cl-annot:enable-annot-syntax)

(defvar *handler* nil)

@export
(defun run (&key (port 18888)
                 (conf (vase.context.configure:load-configure)))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (-> (make-instance 'ningle:<app>)
             (bind-resources! (namestring *default-pathname-defaults*))
             (bind-api! :conf conf)
             (bind-html!))
         :port port)))
