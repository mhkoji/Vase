(ns vase.infra.api.req
  (:require [cljs.core.async :refer [put! chan]]))

(defn to-image [map]
  {:image-id (map "image-id")
   :url (map "url")})

(defn to-folder [map]
  {:folder-id (map "folder-id")
   :name (map "name")
   :thumbnail (to-image (map "thumbnail"))})

(defn to-tag [map]
  {:tag-id (map "tag-id")
   :name (map "name")})

(defn req
  ([method path]
   (req method path {}))
  ([method path opts]
   (let [ch (chan)]
     (method (str "/api" path)
             (merge
              {:format :json
               :handler (fn [obj] (put! ch (obj "result")))}
              opts))
     ch)))
