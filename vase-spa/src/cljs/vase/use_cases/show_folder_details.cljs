(ns vase.use-cases.show-folder-details
  (:require [vase.api.folder :as folder-api]
            [vase.entities.folder]))

(defn get-name [folder-id k]
  (folder-api/get-by-id folder-id (fn [folder]
    (k (-> folder :name)))))

(defn list-images [folder k]
  (when (not (-> folder :images))
    (folder-api/list-images (-> folder :folder-id) (fn [images]
      (k images)))))


(defn update-field [context key fn]
  ((-> context :update-context) #(update % key fn)))

(defrecord Overview [name images])

(defrecord OverviewContext [update-context folder-id overview])

(defn show-overview [overview-context]
  ;; Name
  (when (not (-> overview-context :overview :name))
    (get-name (-> overview-context :folder-id) (fn [name]
      (update-field overview-context :overview (fn [overview]
        (assoc-in overview [:name] name))))))
  ;; Images
  (when (not (-> overview-context :overview :images))
    (list-images (-> overview-context :folder-id) (fn [images]
      (update-field overview-context :overview (fn [overview]
        (assoc-in overview [:images] images)))))))


(defrecord Viewer [thumbnails images index size])

(defn take-thumbnails [images index width]
  (let [count (count images)
        half-width (/ width 2)]
    (cond (<= count width)
          (take count images)
          (< index half-width)
          (take width images)
          (< count (+ index (- half-width) width))
          (take width (drop images))
          :else
          (take width (drop (- index half-width) images)))))

(defrecord ViewerContext [update-context folder-id viewer])

(defn show-viewer [viewer-context]
  (when (not (-> viewer-context :viewer :images))
    (list-images (-> viewer-context :folder-id) (fn [images]
      (update-field viewer-context :viewer (fn [viewer]
        (-> viewer
            (assoc :images images)
            (assoc :index 0)
            (assoc :thumbnail (take-thumbnails images 0 3)))))))))

(defn increment-index [viewer-context diff]
  (let [added-index (+ (-> viewer-context :viewer :index) diff)]
    (update-field viewer-context :viewer (fn [viewer]
      (let [images (-> viewer :images)
            max-index (dec (count images))
            new-index (cond (< added-index 0) 0
                            (< max-index added-index) max-index
                            :else added-index)]
        (-> viewer
            (assoc :index new-index)
            (assoc :thumbnails (take-thumbnails images new-index 3))))))))

(defn resize [viewer-context size]
  (update-field viewer-context :viewer #(assoc % :size size)))
