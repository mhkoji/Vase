(asdf:defsystem :cocoa
  :pathname #P"src/cl/"
  :serial t
  :components
  ((:file "entity/folder/folder")
   (:file "entity/image/image")
   (:file "entity/tag/tag")

   (:file "util/stream")
   (:file "use-case/folder/inject")
   (:file "use-case/folder/create")
   (:file "use-case/folder/delete")
   (:file "use-case/folder/list")
   (:file "use-case/folder/list-images")
   (:file "use-case/image")
   (:file "use-case/tag/tag")
   (:file "use-case/tag/contents/list")
   (:file "use-case/tag/contents/folder")

   (:file "entity/folder/dao")
   (:file "entity/image/dao")
   (:file "entity/tag/dao")

   (:file "infra/dao/sqlite3/sqlite3")
   (:file "infra/dao/sqlite3/folder")
   (:file "infra/dao/sqlite3/image")
   (:file "infra/dao/sqlite3/tag")

   (:file "infra/file/thumbnail")
   (:file "infra/file/retrieve")

   (:file "controller/context")
   (:file "controller/cli/setup-folder")
   (:file "controller/ningle/json")
   (:file "controller/ningle/html")
   (:file "controller/ningle/bind")
   (:file "controller/ningle/run")
   )

  :depends-on (:proton

               :alexandria
               :anaphora
               :cl-annot
               :cl-arrows
               :cl-who
               :clack
               :ironclad
               :jsown
               :log4cl
               :ningle
               :lack
               :lack-middleware-static
               :local-time))
