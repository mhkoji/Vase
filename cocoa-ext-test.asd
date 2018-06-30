(asdf:defsystem :cocoa-ext-test
  :pathname #P"cocoa-ext/"
  :serial t
  :components
  ((:file "testing/fiveam")
   (:file "testing/sqlite3")
   (:file "src/db/sqlite3/folder/folder-fiveam")
   (:file "test/use-case/folder-fiveam"))
  :depends-on (:cocoa :cocoa-spec :cocoa-ext :fiveam)
  :perform (asdf:test-op (o s)
    (asdf-utils:symbol-call :fiveam :run! :cocoa)))