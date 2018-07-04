(asdf:defsystem :cocoa-gtk
  :serial t
  :components
  ((:file "gtk"))

  :depends-on (:cocoa
               :cl-arrows
               :cl-cffi-gtk))
