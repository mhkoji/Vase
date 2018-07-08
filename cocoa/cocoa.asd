(asdf:defsystem :cocoa
  :serial t
  :pathname #P"src/"
  :components
  ((:file "folder/folder")
   (:file "folder/persist")
   (:file "folder/persist-content")

   (:file "fs/image")
   (:file "fs/image-persist")

   (:file "tag")
   (:file "tag-persist")

   (:file "id")

   (:module use-case
    :pathname "use-case"
    :components
    ((:file "image")
     (:file "folder/thumbnail")
     (:file "folder/content")
     (:file "folder/folder")
     (:file "tag/tag")
     (:file "tag/contents")))

   (:module util
    :pathname "util"
    :components
    ((:file "stream")
     (:file "fs/thumbnail")
     (:file "fs/retrieve"))))

  :depends-on (:alexandria
               :anaphora
               :babel
               :cl-annot
               :cl-arrows
               :cl-fad
               :cl-ppcre
               :ironclad))
