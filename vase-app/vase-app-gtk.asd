(asdf:defsystem :vase-app-gtk
  :serial t
  :pathname "src"
  :components
  ((:file "container")

   (:module :gtk
    :pathname "gtk"
    :components
    ((:file "main"))))
  :depends-on (:vase

               :cl-cffi-gtk))
