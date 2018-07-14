(asdf:defsystem :proton
  :serial t
  :components ((:file "proton"))
  :depends-on (:cl-dbi
               :cl-interpol
               :sqlite))
