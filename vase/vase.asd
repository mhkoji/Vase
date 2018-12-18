(asdf:defsystem :vase
  :serial t
  :pathname "src"
  :components
  ((:file "id")

   (:file "folder/thumbnail/thumbnail")
   (:file "folder/repos/db/folder")
   (:file "folder/repos/db/thumbnail")
   (:file "folder/repos/repos")

   (:file "folder/content/content")
   (:file "folder/content/repos/db")
   (:file "folder/content/repos/repos")

   (:file "folder/folder")

   (:file "image/repos/db")
   (:file "image/repos/repos")
   (:file "image/image")

   (:file "folder/thumbnail/image")
   (:file "folder/content/image")

   (:file "tag/repos/db")
   (:file "tag/repos/repos")
   (:file "tag/contents/repos")
   (:file "tag/tag")
   (:file "tag/contents/contents")

   (:module :db
    :pathname "db"
    :components
    ((:file "db")
     (:file "sqlite3/proton")
     (:file "sqlite3/sqlite3")))

   (:file "folder/repos/sqlite3/folder")
   (:file "folder/repos/sqlite3/thumbnail")
   (:file "folder/content/repos/sqlite3")
   (:file "image/repos/sqlite3")
   (:file "tag/repos/sqlite3"))

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
