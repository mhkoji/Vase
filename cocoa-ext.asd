(asdf:defsystem :cocoa-ext
  :pathname #P"cocoa-ext/src/"
  :serial t
  :components
  ((:file "third_party/proton/proton")
   (:file "db/sqlite3/sqlite3")
   (:file "db/sqlite3/folder/folder")
   (:file "db/sqlite3/folder/content")
   (:file "db/sqlite3/image")
   (:file "db/sqlite3/tag")

   (:file "fs/thumbnail")
   (:file "fs/retrieve")
   (:file "context"))
  :depends-on (;; For proton
               :cl-dbi
               :cl-interpol
               :sqlite))
