(asdf:defsystem :vase-app-cli
  :serial t
  :pathname "src"
  :components
  ((:file "container")

   (:module :cli
    :pathname "cli"
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
       (:file "cli"))))))

  :depends-on (:vase

               :log4cl))
