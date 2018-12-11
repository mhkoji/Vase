(asdf:defsystem :vase
  :serial t
  :pathname "src"
  :components
  ((:file "id")

   (:file "folder/repos/db/folder")
   (:file "folder/repos/db/thumbnail")
   (:file "folder/thumbnail/repos")
   (:file "folder/repos/repos")

   (:file "folder/content/repos/db")
   (:file "folder/content/entities/repos")
   (:file "folder/content/repos/repos")

   (:file "folder/folder")

   (:file "image/repos/db")
   (:file "image/repos/repos")
   (:file "image/image")

   (:file "folder/thumbnail/image")
   (:file "folder/content/entities/entities")

   (:file "tag/repos/db")
   (:file "tag/repos/repos")
   (:file "tag/contents/repos")
   (:file "tag/tag")
   (:file "tag/contents/contents")

   (:module :db/sqlite3
    :pathname "db/sqlite3"
    :components
    ((:file "proton")
     (:file "sqlite3")))

   (:file "folder/repos/sqlite3/folder")
   (:file "folder/repos/sqlite3/thumbnail")
   (:file "folder/content/repos/sqlite3")
   (:file "image/repos/sqlite3")
   (:file "tag/repos/sqlite3")

   (:module :context
    :pathname "context"
    :components
    ((:file "configure")
     (:file "context"))))

  :depends-on (; For proton
               :cl-dbi
               :cl-interpol
               :sqlite

               :alexandria
               :anaphora
               :babel
               :cl-annot
               :cl-arrows
               :cl-fad
               :cl-ppcre
               :ironclad))
