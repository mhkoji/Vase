(ns cocoa.use-case.load-folders
  (:require [cljs.core.async :refer [go <! timeout]]
            [cocoa.infra.api.folder :as folder-api]
            [cocoa.entity.folder :as folder]))

(defn cascading-concat [update-repos! folders]
  (go
    (loop [[sub-folders & rest] (partition 100 100 [] folders)]
      (when (not (empty? sub-folders))
        (update-repos! (fn [repos]
          (folder/add-all repos sub-folders)))
        (<! (timeout 100))
        (recur rest)))))

(defn load-all [update-repos! {:keys [from size]}]
  (update-repos! folder/clear)
  (go (let [folders (<! (folder-api/list-by-range from size))]
        (cascading-concat update-repos! folders))))

(defn load-by-tag [update-repos! tag-id]
  (update-repos! folder/clear)
  (go (let [folders (if tag-id
                      (<! (folder-api/list-by-tag tag-id))
                      (<! (folder-api/list-by-range 0 100)))]
        (cascading-concat update-repos! folders))))
