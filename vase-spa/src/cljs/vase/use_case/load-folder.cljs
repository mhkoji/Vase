(ns vase.use-case.load-folder
  (:require [cljs.core.async :refer [go <! timeout]]
            [vase.infra.api.folder :as folder-api]
            [vase.entity.folder :as folder]))

(defn get-name [folder k]
  (when (not (-> folder :name))
    (go (let [folder (<! (folder-api/get-by-id (-> folder :folder-id)))]
          (k (-> folder :name))))))

(defn list-images [folder k]
  (when (not (-> folder :images))
    (go (k (<! (folder-api/list-images (-> folder :folder-id)))))))

(defn load-folder [repos update-repos! folder-id]
  (let [folder (folder/find-folder-by-id repos folder-id)]
    ;; Name
    (get-name folder (fn [name]
      (update-repos! (fn [repos]
        (let [folder (folder/find-folder-by-id repos folder-id)
              updated-folder (folder/set-name folder name)]
          (folder/save-folder repos updated-folder))))))
    ;; Images
    (list-images folder (fn [images]
      (update-repos! (fn [repos]
        (let [folder (folder/find-folder-by-id repos folder-id)
              updated-folder (folder/set-images folder images)]
          (folder/save-folder repos updated-folder))))))))
