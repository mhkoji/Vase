(asdf:defsystem :cocoa
  :serial t
  :components
  ((:module src
    :pathname "src/"
    :components
    ((:module entity
      :pathname "entity"
      :components
      ((:file "folder/folder")
       (:file "folder/persist")
       (:file "folder/persist-content")

       (:file "fs/image")
       (:file "fs/image-persist")

       (:file "tag")
       (:file "tag-persist")

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

   (:module src-db
    :pathname "src/db"
    :components
    ((:file "sqlite3/sqlite3")
     (:file "sqlite3/folder/folder")
     (:file "sqlite3/folder/content")
     (:file "sqlite3/image")
     (:file "sqlite3/tag"))))

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
