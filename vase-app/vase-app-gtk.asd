(asdf:defsystem :vase-app-gtk
  :serial t
  :pathname "src"
  :components
  ((:file "main"))
  :depends-on (:vase

               :cl-cffi-gtk))
