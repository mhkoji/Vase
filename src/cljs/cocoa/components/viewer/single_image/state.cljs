(ns cocoa.components.viewer.single-image.state)

(defn thumbnail-state [id src link-to highlighted-p]
  {:id id
   :src src
   :link-to link-to
   :highlighted-p highlighted-p})

(defn progress-state [now max]
  {:now now :max max})

(defn state [& {:keys [size image-url thumbnails progress on-diff]}]
  {:size        size
   :image-url   image-url
   :thumbnails  thumbnails
   :progress    progress
   :on-diff     on-diff})
