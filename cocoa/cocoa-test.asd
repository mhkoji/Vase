(asdf:defsystem :cocoa-test
  :serial t
  :pathname #P"src/"
  :components
  ((:file "entity/folder/folder-spec")

   (:file "testing/fiveam")
   (:file "infra/db/sqlite3/folder/folder-fiveam"))
  :depends-on (:cocoa :fiveam)
  :perform (asdf:test-op (o s)
    (asdf-utils:symbol-call :fiveam :run! :cocoa)))
