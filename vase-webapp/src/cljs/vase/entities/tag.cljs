(ns vase.entities.tag)

(defrecord Tag [tag-id name])

(defrecord Db [tags])

(defn load-by-id [db tag-id]
  (first (filter #(= tag-id (-> % :tag-id)) (-> db :tags))))

(defn load-by-ids [db tag-ids]
  ;; O(n^2)...
  (map #(load-by-id db %) tag-ids))

(defn load-all [db]
  (:tags db))
