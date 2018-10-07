(ns vase.api.folder
  (:require [cljs.core.async :refer [chan go <! pipe]]
            [ajax.core :refer [GET POST]]
            [vase.api.req :as req]))

(defn list-by-range [from size k]
  (go (let [xs (<! (req/req GET "/folders"
                            {:params {"from" from "size" size}}))]
        (k (map req/obj->folder xs)))))

(defn list-by-tag [tag-id k]
  (go (let [xs (<! (req/req GET (str "/tag/" tag-id "/folders")))]
        (k (map req/obj->folder xs)))))

(defn get-by-id [folder-id k]
  (go (let [x (<! (req/req GET (str "/folder/" folder-id)))]
        (k (req/obj->folder x)))))


(defn list-tags [folder-id k]
  (go (let [xs (<! (req/req GET (str "/folder/" folder-id "/tags")))]
        (k (map req/obj->tag xs)))))

(defn post-tags [folder-id tags k]
  (go (<! (req/req POST (str "/folder/" folder-id "/tags")
               {:params {:tag_ids (map :tag-id tags)}}))
      (k)))

(defn list-images [folder-id k]
  (go (let [xs (<! (req/req GET (str "/folder/" folder-id "/images")))]
        (k (map req/obj->image xs)))))
