(asdf:defsystem :cocoa-spec
  :serial t
  :pathname #P"cocoa/"
  :components
  ((:file "folder/folder-spec")
   (:file "use-case/folder/spec"))
  :depends-on (:cocoa))
