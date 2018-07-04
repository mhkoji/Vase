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

   (:file "use-case/image/add")
   (:file "use-case/image/get-path")
   (:file "use-case/folder/thumbnail")
   (:file "use-case/folder/content")
   (:file "use-case/folder/folder")
   (:file "use-case/folder/add")
   (:file "use-case/folder/add-bulk-by-dirs")
   (:file "use-case/folder/get-by-id")
   (:file "use-case/folder/list-by-ids")
   (:file "use-case/folder/list-by-range")
   (:file "use-case/folder/search-by-name")
   (:file "use-case/folder/delete-by-id")
   (:file "use-case/folder/list-images")
   (:file "use-case/folder/append-contents")
   (:file "use-case/folder/change-thumbnail")
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
