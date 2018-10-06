(ns vase.use-case.load-tags
  (:require [cljs.core.async :refer [go <! timeout]]
            [vase.entity.tag :as tag]
            [vase.infra.api.tag :as tag-api]))

(defn load-all [update-tag!]
  (go (let [tags (<! (tag-api/list-tags))]
        (update-tag! (fn [repos]
          (tag/add-all repos tags))))))

(defn change-name [update-name! name]
  (update-name! (fn [_] name)))

(defn submit-name [update-tag! update-name! tag-id name]
  (go (<! (tag-api/set-name tag-id name))
      (update-tag! (fn [repos]
        (let [tag (tag/find-tag repos tag-id)
              new-tag (tag/set-name tag name)]
          (tag/save-tag repos new-tag))))
      (update-name! (fn [_] nil))))

(defn cancel-change [update-name!]
  (update-name! (fn [_] nil)))
