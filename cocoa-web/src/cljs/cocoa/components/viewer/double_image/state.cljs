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

(defn viewer-select [& {:keys [id name link]}]
  {:id id :name name :link link})

(defn state [& {:keys [size
                       spread-urls
                       thumbnails
                       viewer-select-list
                       progress
                       on-diff]}]
  {:size size
   :spread-urls spread-urls
   :thumbnails thumbnails
   :viewer-select-list viewer-select-list
   :progress progress
   :on-diff on-diff})
