(ns vase.infra.api.folder
  (:require [cljs.core.async :refer [chan go <! pipe]]
            [ajax.core :refer [GET POST]]
            [vase.infra.api.req :as req]))

(defn list-by-range [from size]
  (-> (req/req GET "/folders" {:params {"from" from "size" size}})
      (pipe (chan 1 (map (fn [xs]
                           (map (fn [x] (req/to-folder x)) xs)))))))

(defn ^:export $list-by-range [from size callback]
  (go (callback (clj->js (<! (list-by-range from size))))))

(defn list-by-tag [tag-id]
  (-> (req/req GET (str "/tag/" tag-id "/folders"))
      (pipe (chan 1 (map (fn [xs]
                           (map (fn [x] (req/to-folder x)) xs)))))))


(defn get-by-id [folder-id]
  (-> (req/req GET (str "/folder/" folder-id))
      (pipe (chan 1 (map req/to-folder)))))


(defn list-tags [folder-id]
  (-> (req/req GET (str "/folder/" folder-id "/tags"))
      (pipe (chan 1 (map (fn [xs] (map (fn [x] (req/to-tag x)) xs)))))))

(defn post-tags [folder-id tag-ids]
  (req/req POST (str "/folder/" folder-id "/tags")
           {:params {:tag_ids tag-ids}}))

(defn list-images [folder-id]
  (-> (req/req GET (str "/folder/" folder-id "/images"))
      (pipe (chan 1 (map (fn [xs] (map (fn [x] (req/to-image x)) xs)))))))
