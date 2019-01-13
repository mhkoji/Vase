(asdf:defsystem :vase
  :serial t
  :pathname "src"
  :components
  ((:file "id")

   (:file "folder/thumbnail/thumbnail")

   (:file "folder/content/content")
   (:file "db/folder-content")
   (:file "folder/content/repos")

   (:file "folder/folder")
   (:file "db/folder")
   (:file "db/folder-thumbnail")
   (:file "folder/repos")

   (:file "image/image")
   (:file "db/image")
   (:file "image/repos")

   (:file "folder/thumbnail/image")

   (:file "tag/contents/contents")
   (:file "tag/tag")
   (:file "db/tag")
   (:file "tag/repos")

   (:file "tag/contents/concrete")
   (:file "folder/content/concrete")

   (:file "db/db")
   (:module :db/sqlite3
    :pathname "db/sqlite3"
    :components
    ((:file "proton")
     (:file "sqlite3")
     (:file "folder")
     (:file "folder-thumbnail")
     (:file "folder-content")
     (:file "image")
     (:file "tag"))))

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
