(asdf:defsystem :vase
  :serial t
  :pathname "src"
  :components
  ((:file "id")

   (:file "folder/thumbnail/repos")
   (:file "db/folder/folder")
   (:file "db/folder/thumbnail")
   (:file "folder/db")

   (:file "folder/content/repos")
   (:file "db/folder/content")
   (:file "folder/content/db")

   (:file "folder/folder")

   (:file "db/image")
   (:file "image")

   (:file "folder/thumbnail/image")
   (:file "folder/content/entities")

   (:file "db/tag")
   (:file "tag/contents/repos")
   (:file "tag/db")
   (:file "tag/tag")
   (:file "tag/contents/contents")

   (:module :db/sqlite3
    :pathname "db/sqlite3"
    :components
    ((:file "proton")
     (:file "sqlite3")
     (:file "folder/folder")
     (:file "folder/thumbnail")
     (:file "folder/content")
     (:file "image")
     (:file "tag")))

   (:module :contexts
    :pathname "contexts"
    :components
    ((:file "configure")
     (:file "contexts"))))

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
