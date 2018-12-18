(defpackage :vase.app.web
  (:use :cl :vase.app.web.bind)
  (:export :run)
  (:import-from :cl-arrows :->)
  (:import-from :vase.app.container
                :load-configure))
(in-package :vase.app.web)

(defvar *handler* nil)

(defun run (&key (port 18888)
                 (conf (load-configure)))
  (when *handler*
    (clack:stop *handler*))
  (setq *handler*
        (clack:clackup
         (-> (make-instance 'ningle:<app>)
             (bind-resources! (namestring *default-pathname-defaults*))
             (bind-api! :conf conf)
             (bind-html!))
         :port port)))
