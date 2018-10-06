(asdf:defsystem :vase-test
  :serial t
  :pathname "t"
  :components
  ((:module scenario
    :pathname "scenario"
    :components
    ((:module entities
      :pathname "entities"
      :components
      ((:file "folder/folder")
       (:file "folder/db")))
     (:file "folder")
     (:file "tag")))

   (:module testing
    :pathname "testing"
    :components
    ((:file "sqlite3")))

   (:file "fiveam"))

  :depends-on (:vase :fiveam)

  :perform (asdf:test-op (o s)
    (funcall (intern (symbol-name :run!) :fiveam) :vase)))
