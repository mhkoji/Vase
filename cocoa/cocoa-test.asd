(asdf:defsystem :cocoa-test
  :serial t
  :components
  ((:module src
    :pathname "src"
    :components
    ((:file "entity/folder/db-spec")
     (:file "entity/folder/folder-spec")
     (:file "folder/folder-spec")))

   (:module test
    :pathname "test"
    :components
    ((:file "testing/fiveam")
     (:file "testing/sqlite3")
     (:file "test"))))

  :depends-on (:cocoa :fiveam)

  :perform (asdf:test-op (o s)
    (asdf-utils:symbol-call :fiveam :run! :cocoa)))
