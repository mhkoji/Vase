(ns vase.gui.browser.pages.folder.drivers.viewer
  (:require [vase.entities.transaction :refer [state]]
            [vase.use-cases.show-folder-details]

            [vase.gui.components.header.state
             :as header-state]
            [vase.gui.components.viewer.single-image.state
             :as single-image-viewer-state]
            [vase.gui.components.viewer.double-image.state
             :as double-image-viewer-state]
            [vase.gui.components.header.reagent
             :refer [header]]
            [vase.gui.components.tag-edit-button.reagent
             :refer [tag-edit-button]]
            [vase.gui.components.tag-editing.reagent
             :refer [modal-editing-tag]]
            [vase.gui.components.viewer.single-image.reagent
             :refer [single-image-viewer]]
            [vase.gui.components.viewer.double-image.reagent
             :refer [double-image-viewer]]
            [vase.gui.components.pages.folder
             :refer [viewer-page]]

            [vase.gui.browser.url :as url]
            [reagent.core :as r]))

(defn create-store [update-store folder-id images]
  {:show-folder-details
   (vase.entities.transaction/DelegateTransaction.
    (vase.use-cases.show-folder-details/ViewerState.
     folder-id nil images 0 nil)
    #(update-store
      (fn [s] (update-in s [:show-folder-details :state] %))))})

(defn store-header-state [store]
  (when (not (-> store :show-folder-details state :images))
    (header-state/get-state :folder)))

(defn store-single-state [store]
  (let [state (-> store :show-folder-details state)]
    (when-let [images (-> state :images)]
      (let [index  (-> state :index)
            count (count images)
            folder-id (-> state :folder-id)]
        (single-image-viewer-state/state
         :size
         (-> state :size)

         :image-url
         (-> (nth images index) :url)

         :thumbnails
         (let [highlighted-id (:image-id (nth images index))]
           (for [thumbnail (-> state :thumbnails)]
             (let [id (-> thumbnail :image-id)]
               (single-image-viewer-state/thumbnail-state
                id
                (-> thumbnail :url)
                (url/read-folder-by-spread folder-id id)
                (= id highlighted-id)))))

         :progress
         (single-image-viewer-state/progress-state index count)

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
         #(vase.use-cases.show-folder-details/increment-index
           (-> store :show-folder-details) %))))))

(defn store-double-state [store]
  (let [state (-> store :show-folder-details state)]
    (when-let [images (-> state :images)]
      (let [index (-> state :index)
            count (count images)
            folder-id (-> state :folder-id)]
        (double-image-viewer-state/state
         :size
         (-> state :size)

         :spread-urls
         (cond (= index 0)
               (double-image-viewer-state/spread-urls-state
                (-> (nth images 0) :url)
                "")
               (= index (- count 1))
               (double-image-viewer-state/spread-urls-state
                (-> (nth images (- count 1)) :url)
                "")
               :else
               (let [idx (if (= (rem index 2) 1) index (- index 1))]
                 (double-image-viewer-state/spread-urls-state
                  (-> (nth images (+ 1 idx)) :url)
                  (-> (nth images idx)       :url))))

         :viewer-select-list
         [(double-image-viewer-state/viewer-select
           :id "double" :name "Double" :link "#")
          (double-image-viewer-state/viewer-select
           :id "single" :name "Single"
           :link (url/read-folder-by-single
                  folder-id
                  (-> images (nth index) :image-id)))
          (double-image-viewer-state/viewer-select
           :id "list" :name "List" :link (url/folder folder-id))]

         :thumbnails
         (let [highlighted-id (:image-id (nth images index))]
           (for [thumbnail (-> state :thumbnails)]
             (let [id (-> thumbnail :image-id)]
               (double-image-viewer-state/thumbnail-state
                id
                (-> thumbnail :url)
                (url/read-folder-by-spread folder-id id)
                (= id highlighted-id)))))

         :progress
         (double-image-viewer-state/progress-state index count)

         :on-diff
         #(vase.use-cases.show-folder-details/increment-index
           (-> store :show-folder-details) %))))))


(defn create-double [elem folder-id]
  {:create-store
   (fn [update-store]
     (let [store (create-store update-store folder-id nil)]
       (vase.use-cases.show-folder-details/show-viewer
        (-> store :show-folder-details))
       store))
   :render
   (fn [store]
     (r/render
      [viewer-page
       {:header
        (when-let [header-state (store-header-state store)]
          (fn [] [header header-state]))
        :render
        (when-let [viewer-state (store-double-state store)]
          (fn [] [double-image-viewer viewer-state]))
        :on-resize-window
        #(vase.use-cases.show-folder-details/resize
          (-> store :show-folder-details) %)}]
      elem))})

(defn create-single [elem folder-id]
  {:create-store
   (fn [update-store]
     (let [store (create-store update-store folder-id nil)]
       (vase.use-cases.show-folder-details/show-viewer
        (-> store :show-folder-details))
       store))
   :render
   (fn [store]
     (r/render
      [viewer-page
       {:header
        (when-let [header-state (store-header-state store)]
          (fn [] [header header-state]))
        :render
        (when-let [viewer-state (store-single-state store)]
          (fn [] [single-image-viewer viewer-state]))
        :on-resize-window
        #(vase.use-cases.show-folder-details/resize
          (-> store :show-folder-details) %)}]
      elem))})
