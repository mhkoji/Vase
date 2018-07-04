(asdf:defsystem :cocoa
  :serial t
  :components
  ((:file "folder/folder")
   (:file "folder/persist")
   (:file "folder/persist-content")

   (:file "fs/image")
   (:file "fs/image-persist")

   (:file "tag")
   (:file "tag-persist")

   (:file "use-case/image")
   (:file "use-case/folder/thumbnail")
   (:file "use-case/folder/content")
   (:file "use-case/folder/folder")
   (:file "use-case/tag/tag")
   (:file "use-case/tag/contents/list")
   (:file "use-case/tag/contents/folder")

   (:file "util/stream"))

  :depends-on (:alexandria
               :anaphora
               :cl-annot
               :cl-arrows
               :cl-fad
               :cl-ppcre
               :ironclad))
