(asdf:defsystem :cocoa-db
  :serial t
  :components
  ((:module third-party
    :pathname "third_party"
    :components
    ((:file "proton/proton")))

   (:module src
    :pathname #P"src/"
    :components
    ((:file "sqlite3/sqlite3")
     (:file "sqlite3/folder/folder")
     (:file "sqlite3/folder/content")
     (:file "sqlite3/image")
     (:file "sqlite3/tag"))))

  :depends-on (;; For proton
               :cl-dbi
               :cl-interpol
               :sqlite))
