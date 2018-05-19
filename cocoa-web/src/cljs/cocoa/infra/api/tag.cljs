(ns cocoa.infra.api.tag
  (:require [ajax.core :refer [GET POST DELETE PUT]]
            [cljs.core.async :refer [chan pipe put! go <!]]
            [cocoa.infra.api.req :as req]))

(defn list-tags []
  (let [ch (chan 1)]
    (go (let [xs (<! (req/req GET "/tags"))]
          (put! ch (map req/to-tag xs))))
    ch))

(defn add-tag [name]
  (req/req POST "/tags" {:params {:name name}}))

(defn set-name [tag-id name]
  (req/req PUT (str "/tag/" tag-id "?name=" name)))

(defn delete-tag [tag-id]
  (req/req DELETE (str "/tag/" tag-id)))
