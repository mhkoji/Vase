(defun main ()
  (let ((context-path (second sb-ext:*posix-argv*)))
    (log:info "Context path: ~A" context-path)
    (vase.spa:run :port 11111
                  :context (vase:load-context context-path)))
  (swank-loader:init)
  (swank:create-server :port 22222 :style :spawn :dont-close t))

(main)
