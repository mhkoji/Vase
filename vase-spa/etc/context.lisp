#.(vase:make-context
   :id-generator
   (make-instance 'vase.entities.id:sha256)

   :connection-factory
   (make-instance 'proton:sqlite3-factory :db-path "/app/db.sqlite.bin")

   :thumbnail-root "/app/thumbnails/")
