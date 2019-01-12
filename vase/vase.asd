(asdf:defsystem :vase
  :serial t
  :pathname "src"
  :components
  ((:file "id")

   (:file "folder/thumbnail/thumbnail")

   (:file "folder/content/content")
   (:file "folder/content/db/db")
   (:file "folder/content/repos")

   (:file "folder/folder")
   (:file "folder/db/folder")
   (:file "folder/db/thumbnail")
   (:file "folder/repos")

   (:file "image/image")
   (:file "image/db/db")
   (:file "image/repos")

   (:file "folder/thumbnail/image")

   (:file "tag/contents/contents")
   (:file "tag/tag")
   (:file "tag/db/db")
   (:file "tag/repos")

   (:file "tag/contents/concrete")
   (:file "folder/content/concrete")

   (:module :db
    :pathname "db"
    :components
    ((:file "db")
     (:file "sqlite3/proton")
     (:file "sqlite3/sqlite3")))

   (:file "folder/db/sqlite3/folder")
   (:file "folder/db/sqlite3/thumbnail")
   (:file "folder/content/db/sqlite3")
   (:file "image/db/sqlite3")
   (:file "tag/db/sqlite3"))

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
