(ns vase.gui.components.viewer.single-image.state)

(defn thumbnail-state [id src link-to highlighted-p]
  {:id id
   :src src
   :link-to link-to
   :highlighted-p highlighted-p})

(defn progress-state [now max]
  {:now now :max max})

(defn viewer-select [& {:keys [id name link]}]
  {:id id :name name :link link})

(defn state [& {:keys [size
                       image-url
                       thumbnails
                       viewer-select-list
                       progress
                       on-diff]}]
  {:size  size
   :image-url image-url
   :thumbnails thumbnails
   :viewer-select-list viewer-select-list
   :progress progress
   :on-diff  on-diff})
