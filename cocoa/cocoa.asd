(asdf:defsystem :cocoa
  :serial t
  :components
  ((:module src
    :pathname "src/"
    :components
    ((:module entity
      :pathname "entity"
      :components
      ((:module folder
        :pathname "folder"
        :components
        ((:file "folder")
         (:file "repository")
         (:file "content/content")
         (:file "content/op")
         (:file "content/repository")))

       (:module fs/image
        :pathname "fs/image"
        :components
        ((:file "image")
         (:file "repository")))

       (:module tag
        :pathname "tag"
        :components
        ((:file "tag")
         (:file "repository")))

       (:file "id")))

     (:file "image")
     (:file "folder/util")
     (:file "folder/folder")
     (:file "folder/overview")
     (:file "folder/content")
     (:file "folder/tag")
     (:file "tag/tag")
     (:file "tag/contents")

     (:module util
      :pathname "util"
      :components
      ((:file "stream")
       (:file "fs/thumbnail")
       (:file "fs/retrieve")))))

   (:module third-party
    :pathname "third_party"
    :components
    ((:file "proton/proton")))

   (:module src/db
    :pathname "src/db"
    :components
    ((:file "sqlite3/sqlite3")
     (:file "sqlite3/folder/folder")
     (:file "sqlite3/folder/content")
     (:file "sqlite3/image")
     (:file "sqlite3/tag")))

   (:module src/di
    :pathname "src/di"
    :components
    ((:file "context"))))

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
