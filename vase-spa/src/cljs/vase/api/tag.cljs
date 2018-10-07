(ns vase.api.tag
  (:require [ajax.core :refer [GET POST DELETE PUT]]
            [cljs.core.async :refer [chan pipe put! go <!]]
            [vase.api.req :as req]))

(defn list-tags [k]
  (go (let [xs (<! (req/req GET "/tags"))]
        (k (map req/obj->tag xs)))))

(defn add-tag [name k]
  (go (req/req POST "/tags" {:params {:name name}})
      (k)))

(defn set-name [tag-id name k]
  (go (req/req PUT (str "/tag/" tag-id "?name=" name))
      (k)))

(defn delete-tag [tag-id k]
  (go (req/req DELETE (str "/tag/" tag-id))
      (k)))
