(asdf:defsystem :vase-cli
  :serial t
  :pathname "cl"
  :components
  ((:file "stream")

   (:module :fs
    :pathname "fs"
    :components
    ((:file "packages")
     (:file "thumbnail")
     (:file "retrieve")))

   (:module :cli
    :pathname "cli"
    :components
    ((:file "add-folders")
     (:file "cli"))))

  :depends-on (:vase

               :log4cl))
