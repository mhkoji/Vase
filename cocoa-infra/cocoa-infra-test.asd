(asdf:defsystem :cocoa-infra-test
  :serial t
  :components
  ((:file "testing/fiveam")
   (:file "testing/sqlite3")
   (:file "test/folder-fiveam")
   (:file "test/use-case/folder-fiveam"))
  :depends-on (:cocoa :cocoa-spec :cocoa-infra :fiveam)
  :perform (asdf:test-op (o s)
    (asdf-utils:symbol-call :fiveam :run! :cocoa)))
