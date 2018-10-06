(asdf:defsystem :vase
  :serial t
  :pathname "src"
  :components
  ((:module entities
    :pathname "entities"
    :components
    ((:module folder
      :pathname "folder"
      :components
      ((:file "folder")
       (:file "db")
       (:file "repository")
       (:file "content/content")
       (:file "content/op")
       (:file "content/db")
       (:file "content/repository")))

     (:module fs/image
      :pathname "fs/image"
      :components
      ((:file "image")
       (:file "db")
       (:file "repository")))

     (:module tag
      :pathname "tag"
      :components
      ((:file "tag")
       (:file "db")
       (:file "repository")))

     (:file "id")))

   (:module app
    :pathname "app"
    :components
    ((:file "image")
     (:file "folder/util")
     (:file "folder/folder")
     (:file "folder/overview")
     (:file "folder/content")
     (:file "folder/tag")
     (:file "tag/tag")
     (:file "tag/contents")))

   (:module db/sqlite3
    :pathname "db/sqlite3"
    :components
    ((:file "proton")
     (:file "sqlite3")
     (:file "folder/folder")
     (:file "folder/content")
     (:file "image")
     (:file "tag")))

   (:module util
    :pathname "util"
    :components
    ((:file "stream")
     (:file "fs/thumbnail")
     (:file "fs/retrieve")))

   (:file "vase"))

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
