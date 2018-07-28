(asdf:defsystem :cocoa-test
  :serial t
  :pathname "test"
  :components
  ((:module scenario
    :pathname "scenario"
    :components
    ((:file "entity/folder/folder")
     (:file "entity/folder/db")
     (:file "folder")
     (:file "tag")))

   (:module testing
    :pathname "testing"
    :components
    ((:file "sqlite3")))

   (:file "fiveam"))

  :depends-on (:cocoa :fiveam)

  :perform (asdf:test-op (o s)
    (funcall (intern (symbol-name :run!) :fiveam) :cocoa)))
