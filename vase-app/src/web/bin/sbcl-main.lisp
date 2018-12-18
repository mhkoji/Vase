(defun main ()
  (let ((conf-path (second sb-ext:*posix-argv*)))
    (log:info "Configure path: ~A" conf-path)
    (vase.webapp:run
     :port 11111
     :conf (vase.context.configure:load-configure conf-path)))
  (swank-loader:init)
  (swank:create-server :port 22222 :style :spawn :dont-close t))

(main)
