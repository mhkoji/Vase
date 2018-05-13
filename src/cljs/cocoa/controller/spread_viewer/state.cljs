(ns cocoa.controller.spread-viewer.state)

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
   :on-diff     on-diff})
