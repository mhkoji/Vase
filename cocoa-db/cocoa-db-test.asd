(asdf:defsystem :cocoa-db-test
  :serial t
  :pathname #P"test/"
  :components
  ((:file "testing/fiveam")
   (:file "testing/sqlite3")
   (:file "test"))
  :depends-on (:cocoa :cocoa-spec :cocoa-db :fiveam)
  :perform (asdf:test-op (o s)
    (asdf-utils:symbol-call :fiveam :run! :cocoa)))
