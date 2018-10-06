(asdf:defsystem :vase-cli
  :serial t
  :components
  ((:module add-folders
    :pathname "add-folders"
    :components
    ((:file "stream")
     (:file "fs/thumbnail")
     (:file "fs/retrieve")
     (:file "add-folders"))))

  :depends-on (:vase

               :log4cl))
