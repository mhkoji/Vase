(asdf:defsystem :cocoa-spec
  :serial t
  :components
  ((:file "folder/folder-spec")
   (:file "use-case/folder/spec"))
  :depends-on (:cocoa))
