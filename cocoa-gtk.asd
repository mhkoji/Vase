(asdf:defsystem :cocoa-gtk
  :pathname #P"cocoa-gtk/"
  :serial t
  :components
  ((:file "gtk"))

  :depends-on (:cocoa
               :cl-arrows
               :cl-cffi-gtk))
