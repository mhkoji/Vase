(asdf:defsystem :cocoa-test
  :serial t
  :components
  ((:module src
    :pathname "src"
    :components
    ((:file "entity/folder/db-spec")
     (:file "entity/folder/folder-spec")))

   (:module test
    :pathname "test"
    :components

    ((:module scenario
      :pathname "scenario"
      :components
      ((:file "folder")
       (:file "tag")))

     (:module testing
      :pathname "testing"
      :components
      ((:file "sqlite3")))

     (:module fiveam
      :pathname "fiveam"
      :components
      ((:file "suite")
       (:file "unit")
       (:file "scenario"))))))

  :depends-on (:cocoa :fiveam)

  :perform (asdf:test-op (o s)
    (funcall (intern (symbol-name :run!) :fiveam) :cocoa)))
