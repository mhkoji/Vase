(ns vase.use-cases.show-folders
  (:require [cljs.core.async :refer [go <! timeout]]
            [vase.api.folder :as folder-api]
            [vase.entities.transaction :refer [update-state]]))

(defn call-partitioned [xs callback]
  (go (loop [[sub-xs & rest] (partition 100 100 [] xs)]
        (when (not (empty? sub-xs))
          (callback sub-xs)
          (<! (timeout 100))
          (recur rest)))))


(defrecord Region [from size])

(defn make-region [from size]
  (Region. (if (< from 0) nil from) size))


(defrecord State [folders prev-region next-region])

(defn show [transaction region]
  (let [{:keys [from size]} region]
    (update-state transaction
      #(-> %
           (assoc :folders nil)
           (assoc :prev-region (make-region (- from size) size))
           (assoc :next-region (make-region (+ from size) size))))
    (folder-api/list-by-range from size (fn [folders]
      (call-partitioned folders (fn [sub]
        (update-state transaction #(update % :folders concat sub))))))))
