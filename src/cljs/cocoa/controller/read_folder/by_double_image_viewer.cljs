(ns cocoa.controller.read-folder.by-double-image-viewer
  (:require [cljs.core.async :refer [go <!]]
            [cocoa.entity.folder :as folder]
            [cocoa.use-case.load-folder :as load-folder-use-case]
            [cocoa.components.header.state :as header-state]
            [cocoa.components.viewer.double-image.state
             :as double-image-viewer-state]
            [cocoa.presenter.browser.url :as url]))

(defn create-store [update-store! folder-id images]
  {:const
   {:folder-id folder-id}
   :db
   {:folder (folder/save-folder
             (folder/repository. [])
             {:folder-id folder-id :images images})
    :viewer-size nil
    :index 0}
   :update-db
   {:folder
    #(update-store! (fn [s] (update-in s [:db :folder] %)))
    :index
    #(update-store! (fn [s] (update-in s [:db :index] %)))
    :viewer-size
    #(update-store! (fn [s] (update-in s [:db :viewer-size] %)))}})

(defn set-size [update-size! w h]
  (update-size! (fn [_]
    {:width w :height h})))

(defn increment-index [update-index! length diff]
  (update-index! (fn [current-index]
    (let [new-index (+ current-index diff)]
      (cond (< new-index 0)       0
            (<= length new-index) (- length 1)
            :else                 new-index)))))

(defn thumbnail-range [index length width]
  (let [half-width (/ width 2)]
    (cond (<= length width)
          {:begin 0 :end length}
          (< index half-width)
          {:begin 0 :end width}
          (< length (+ index (- half-width) width))
          {:begin (- length width) :end length}
          :else
          (let [begin (- index half-width)]
            {:begin begin :end (+ begin width)}))))

(defn store-viewer-state [store]
  (let [folder-id (-> store :const :folder-id)
        folder-repos (-> store :db :folder)
        folder (folder/find-folder-by-id folder-repos folder-id)]
    (when-let [images (-> folder :images)]
      (let [index  (-> store :db :index)
            length (count images)]
        (double-image-viewer-state/state
         :size
         (-> store :db :viewer-size)

         :spread-urls
         (cond (= index 0)
               (double-image-viewer-state/spread-urls-state
                (-> (nth images 0) :url)
                "")
               (= index (- length 1))
               (double-image-viewer-state/spread-urls-state
                (-> (nth images (- length 1)) :url)
                "")
               :else
               (let [idx (if (= (rem index 2) 1)
                           index
                           (- index 1))]
                 (double-image-viewer-state/spread-urls-state
                  (-> (nth images (+ 1 idx)) :url)
                  (-> (nth images idx)       :url))))

         :thumbnails
         (let [highlighted-id (:image-id (nth images index))]
           (for [im (let [{:keys [begin end]}
                          (thumbnail-range index length 3)]
                      (take (- end begin) (drop begin images)))]
             (let [id (-> im :image-id)]
               (double-image-viewer-state/thumbnail-state
                id
                (-> im :url)
                (url/read-folder-by-spread folder-id id)
                (= id highlighted-id)))))

         :progress
         (double-image-viewer-state/progress-state index length)

         :on-diff
         #(increment-index (-> store :update-db :index) length %))))))

(defn store-header-state [store]
  (let [folder-id    (-> store :const :folder-id)
        folder-repos (-> store :db :folder)
        folder (folder/find-folder-by-id folder-repos folder-id)]
    (when (not (-> folder :images))
      (header-state/get-state :folder))))

(defn store-load-images! [store]
  (load-folder-use-case/load-folder (-> store :db :folder)
                                    (-> store :update-db :folder)
                                    (-> store :const :folder-id)))

(defn store-resize! [store width height]
  (set-size (-> store :update-db :viewer-size) width height))
