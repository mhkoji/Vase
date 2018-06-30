(asdf:defsystem :cocoa-web
  :pathname #P"cocoa-web/src/cl/"
  :serial t
  :components
  ((:file "json")
   (:file "html")
   (:file "bind")
   (:file "run"))
  :depends-on (:cocoa
               :cocoa-ext

               :cl-who
               :cl-arrows
               :clack
               :jsown
               :log4cl
               :lack
               :lack-middleware-static
               :ningle))