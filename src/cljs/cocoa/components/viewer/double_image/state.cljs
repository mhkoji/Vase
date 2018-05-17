(ns cocoa.components.viewer.double-image.state)

(defn spread-urls-state [left-url right-url]
  {:left left-url :right right-url})

(defn progress-state [now max]
  {:now now :max max})

(defn thumbnail-state [id src link-to highlighted-p]
  {:id id
   :src src
   :link-to link-to
   :highlighted-p highlighted-p})

(defn state [& {:keys [size spread-urls thumbnails progress on-diff]}]
  {:size        size
   :spread-urls spread-urls
   :thumbnails  thumbnails
   :progress    progress
   :on-diff     on-diff})