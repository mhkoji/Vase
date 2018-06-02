(asdf:defsystem :cocoa-test
  :serial t
  :pathname #P"src/"
  :components
  ((:file "entity/folder/folder-spec")
   (:file "use-case/folder/spec")

   (:file "testing/fiveam")
   (:file "testing/sqlite3")
   (:file "infra/db/sqlite3/folder/folder-fiveam")
   (:file "use-case/folder/spec-fiveam"))
  :depends-on (:cocoa :fiveam)
  :perform (asdf:test-op (o s)
    (asdf-utils:symbol-call :fiveam :run! :cocoa)))
