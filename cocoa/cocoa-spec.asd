(asdf:defsystem :cocoa-spec
  :serial t
  :components
  ((:module src
    :pathname "src"
    :components
    ((:file "folder/folder-spec")))

   (:module spec
    :pathname "spec"
    :components
    ((:file "folder"))))
  :depends-on (:cocoa))
