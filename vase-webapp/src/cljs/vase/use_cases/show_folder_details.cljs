(ns vase.use-cases.show-folder-details
  (:require [vase.api.folder :as folder-api]
            [vase.entities.folder]
            [vase.entities.transaction :refer [state update-state]]))

(defn get-name [folder-id k]
  (folder-api/get-by-id folder-id #(k (-> % :name))))

(defn list-images [folder-id k]
  (folder-api/list-images folder-id #(k %)))


(defrecord OverviewState [folder-id name images])

(defn show-overview [transaction]
  ;; Name
  (when (not (-> transaction state :name))
    (get-name (-> transaction state :folder-id) (fn [name]
      (update-state transaction #(assoc % :name name)))))
  ;; Images
  (when (not (-> transaction state :images))
    (list-images (-> transaction state :folder-id) (fn [images]
      (update-state transaction #(assoc % :images images))))))


(defrecord ViewerState [folder-id thumbnails images index size])

(defn take-thumbnails [images index width]
  (let [count (count images)
        half-width (/ width 2)]
    (cond (<= count width)
          (take count images)
          (< index half-width)
          (take width images)
          (< count (+ index (- half-width) width))
          (take width (drop (- count width) images))
          :else
          (take width (drop (- index half-width) images)))))

(defn show-viewer [transaction]
  (let [state (state transaction)]
    (when (not (-> state :images))
      (list-images (-> state :folder-id) (fn [images]
        (update-state transaction
          #(-> %
               (assoc :images images)
               (assoc :index 0)
               (assoc :thumbnails (take-thumbnails images 0 3)))))))))

(defn increment-index [transaction diff]
  (update-state transaction (fn [state]
    (let [images (-> state :images)
          added-index (+ (-> state :index) diff)
          max-index (dec (count images))
          new-index (cond (< added-index 0) 0
                          (< max-index added-index) max-index
                          :else added-index)]
      (-> state
          (assoc :index new-index)
          (assoc :thumbnails (take-thumbnails images new-index 3)))))))

(defn resize [transaction size]
  (update-state transaction #(assoc % :size size)))
