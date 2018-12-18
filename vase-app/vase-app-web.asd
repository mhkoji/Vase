(asdf:defsystem :vase-app-web
  :pathname "src"
  :serial t
  :components
  ((:file "container")

   (:module :web
    :pathname "web/src/cl"
    :components
    ((:file "json")
     (:file "html")
     (:file "bind")
     (:file "run"))))
  :depends-on (:vase

               :cl-who
               :cl-arrows
               :clack
               :jsown
               :log4cl
               :lack
               :lack-middleware-static
               :ningle))
