(asdf:defsystem :cocoa-gtk
  :pathname #P"src/"
  :serial t
  :components
  ((:file "gtk"))

  :depends-on (:cocoa :cl-cffi-gtk))
