(asdf:defsystem :vase-webapp
  :pathname #P"src/cl/"
  :serial t
  :components
  ((:file "json")
   (:file "html")
   (:file "bind")
   (:file "run"))
  :depends-on (:vase

               :cl-who
               :cl-arrows
               :clack
               :jsown
               :log4cl
               :lack
               :lack-middleware-static
               :ningle))
