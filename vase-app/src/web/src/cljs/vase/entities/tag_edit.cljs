(ns vase.entities.tag-edit
  (:require [vase.entities.tag]))

(defrecord TagEdit [content attached-tags])

(defn attach [edit tag]
  (update edit :attached-tags conj tag))

(defn detach [edit tag]
  (let [tag-id (-> tag :tag-id)]
    (update edit :attached-tags
            #(remove (fn [tag]
                       (= tag-id (-> tag :tag-id)))
                     %))))

(defn attached-tag-p [edit tag]
  (let [tag-id (-> tag :tag-id)
        attached-ids (map :tag-id (:attached-tags edit))]
    (not (empty? (filter #(= % tag-id) attached-ids)))))
