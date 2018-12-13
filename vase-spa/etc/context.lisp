#.(vase.context.configure:make-configure
   :id-generator
   (make-instance 'vase.id:sha256)

   :connection-factory
   (make-instance 'proton:sqlite3-factory :db-path "/app/db.sqlite.bin")

   :thumbnail-root "/app/thumbnails/")
