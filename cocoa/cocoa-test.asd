(asdf:defsystem :cocoa-test
  :serial t
  :pathname #P"src/"
  :components
  ((:file "entity/folder/dao-spec")
   (:file "testing/fiveam")
   (:file "infra/dao/sqlite3/folder-test"))
  :depends-on (:cocoa :fiveam)
  :perform (asdf:test-op (o s)
    (asdf-utils:symbol-call :fiveam :run! :cocoa)))
