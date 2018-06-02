(asdf:defsystem :cocoa
  :pathname #P"src/"
  :serial t
  :components
  ((:file "entity/folder/package")
   (:file "entity/folder/thumbnail")
   (:file "entity/folder/folder")
   (:file "entity/folder/content")
   (:file "entity/folder/macro")

   (:file "entity/fs/image")

   (:file "entity/tag/package")
   (:file "entity/tag/content")
   (:file "entity/tag/tag")

   (:file "use-case/image")
   (:file "util/stream")
   (:file "use-case/folder/thumbnail")
   (:file "use-case/folder/content")
   (:file "use-case/folder/package")
   (:file "use-case/folder/add")
   (:file "use-case/folder/delete")
   (:file "use-case/folder/list")
   (:file "use-case/folder/images")
   (:file "use-case/tag/tag")
   (:file "use-case/tag/contents/list")
   (:file "use-case/tag/contents/folder")

   (:file "util/third_party/proton/proton")
   (:file "infra/db/sqlite3/sqlite3")
   (:file "infra/db/sqlite3/folder/folder")
   (:file "infra/db/sqlite3/folder/content")
   (:file "infra/db/sqlite3/image")
   (:file "infra/db/sqlite3/tag")

   (:file "infra/fs/thumbnail")
   (:file "infra/fs/retrieve")
   (:file "infra/context"))

  :depends-on (;; For proton
               :cl-dbi
               :cl-interpol
               :sqlite
               ;; For cocoa
               :alexandria
               :anaphora
               :cl-annot
               :cl-arrows
               :cl-fad
               :ironclad))
