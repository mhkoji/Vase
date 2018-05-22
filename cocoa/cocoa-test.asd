(asdf:defsystem :cocoa-test
  :serial t
  :pathname #P"src/"
  :components
  ((:file "infra/db/folder/dao-spec")

   (:file "testing/fiveam")
   (:file "infra/db/folder/sqlite3-fiveam"))
  :depends-on (:cocoa :fiveam)
  :perform (asdf:test-op (o s)
    (asdf-utils:symbol-call :fiveam :run! :cocoa)))
