(asdf:defsystem :cocoa-web-simple
  :pathname #P"src/cl/"
  :serial t
  :components
  ((:file "view")
   (:file "controller")
   (:file "run"))
  :depends-on (:cocoa
               :cocoa-di

               :cl-who
               :cl-arrows
               :clack
               :jsown
               :log4cl
               :lack
               :lack-middleware-static
               :ningle))
