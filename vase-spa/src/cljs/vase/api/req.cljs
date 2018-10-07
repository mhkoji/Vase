(ns vase.api.req
  (:require [cljs.core.async :refer [put! chan]]
            [vase.entities.folder]
            [vase.entities.tag]))

(defn obj->folder [x]
  (vase.entities.folder/Folder.
   (x "folder-id")
   (x "name")
   (vase.entities.folder/Thumbnail. ((x "thumbnail") "image-id")
                                    ((x "thumbnail") "url"))))

(defn obj->tag [x]
  (vase.entities.tag/Tag. (x "tag-id") (x "name")))

(defn obj->image [map]
  {:image-id (map "image-id")
   :url (map "url")})

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
