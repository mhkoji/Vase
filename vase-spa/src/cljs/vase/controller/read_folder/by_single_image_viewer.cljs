(ns vase.controller.read-folder.by-single-image-viewer
  (:require [cljs.core.async :refer [go <!]]
            [vase.entity.folder :as folder]
            [vase.use-case.load-folder :as load-folder-use-case]
            [vase.components.header.state
             :as header-state]
            [vase.components.viewer.single-image.state
             :as single-image-viewer-state]
            [vase.presenter.browser.url :as url]
            [vase.controller.read-folder.by-double-image-viewer
             :refer [thumbnail-range increment-index set-size]]))

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

(defn store-viewer-state [store]
  (let [folder-id (-> store :const :folder-id)
        folder-repos (-> store :db :folder)
        folder (folder/find-folder-by-id folder-repos folder-id)]
    (when-let [images (-> folder :images)]
      (let [index  (-> store :db :index)
            length (count images)]
        (single-image-viewer-state/state
         :size
         (-> store :db :viewer-size)

         :image-url
         (-> (nth images index) :url)

         :thumbnails
         (let [highlighted-id (:image-id (nth images index))]
           (for [im (let [{:keys [begin end]}
                          (thumbnail-range index length 3)]
                      (take (- end begin) (drop begin images)))]
             (let [id (-> im :image-id)]
               (single-image-viewer-state/thumbnail-state
                id
                (-> im :url)
                (url/read-folder-by-spread folder-id id)
                (= id highlighted-id)))))

         :progress
         (single-image-viewer-state/progress-state index length)

         :viewer-select-list
         [(single-image-viewer-state/viewer-select
           :id "single" :name "Single" :link "#")
          (single-image-viewer-state/viewer-select
           :id "double" :name "Double"
           :link (url/read-folder-by-spread
                  folder-id
                  (-> images (nth index) :image-id)))
          (single-image-viewer-state/viewer-select
           :id "list" :name "List" :link (url/folder folder-id))]

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
