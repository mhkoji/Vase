(asdf:defsystem :cocoa
  :pathname #P"src/"
  :serial t
  :components
  ((:file "entity/folder/folder")
   (:file "entity/folder/content")
   (:file "entity/folder/thumbnail")
   (:file "entity/image/image")
   (:file "entity/tag/tag")

   (:file "use-case/image")
   (:file "util/stream")
   (:file "use-case/folder/add")
   (:file "use-case/folder/delete")
   (:file "use-case/folder/list")
   (:file "use-case/folder/list-images")
   (:file "use-case/tag/tag")
   (:file "use-case/tag/contents/list")
   (:file "use-case/tag/contents/folder")

   (:file "entity/folder/dao")
   (:file "entity/image/dao")
   (:file "entity/tag/dao")

   (:file "util/third_party/proton/proton")
   (:file "infra/dao/sqlite3/sqlite3")
   (:file "infra/dao/sqlite3/folder")
   (:file "infra/dao/sqlite3/image")
   (:file "infra/dao/sqlite3/tag")

   (:file "infra/file/thumbnail")
   (:file "infra/file/retrieve")
   (:file "infra/context"))

  :depends-on (;; For proton
               :cl-dbi
               :cl-interpol
               :sqlite
               ;; For cocoa
               :alexandria
               :anaphora
               :cl-annot
               :cl-fad
               :ironclad))
