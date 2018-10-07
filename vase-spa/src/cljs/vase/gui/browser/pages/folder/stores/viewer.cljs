(ns vase.gui.browser.pages.folder.stores.viewer
  (:require [vase.use-cases.show-folder-details]
            [vase.gui.components.header.state :as header-state]
            [vase.gui.components.viewer.single-image.state
             :as single-image-viewer-state]
            [vase.gui.components.viewer.double-image.state
             :as double-image-viewer-state]
            [vase.gui.browser.url :as url]))

(defn create-store [update-store folder-id images]
  {:context
   (vase.use-cases.show-folder-details/ViewerContext.
    #(update-store (fn [s] (update-in s [:context] %)))
    folder-id
    (vase.use-cases.show-folder-details/Viewer. nil images nil nil))})


(defn store-header-state [store]
  (if (-> store :context :viewer :images)
    nil
    (header-state/get-state :folder)))

(defn store-show-viewer [store]
  (vase.use-cases.show-folder-details/show-viewer (-> store :context)))

(defn store-resize [store width height]
  (vase.use-cases.show-folder-details/resize (-> store :context)
                                             {:width width
                                              :height height}))

(defn store-single-state [store]
  (let [viewer (-> store :context :viewer)]
    (when-let [images (-> viewer :images)]
      (let [index  (-> viewer :index)
            count (count images)
            folder-id (-> store :context :folder-id)]
        (single-image-viewer-state/state
         :size
         (-> viewer :size)

         :image-url
         (-> (nth images index) :url)

         :thumbnails
         (let [highlighted-id (:image-id (nth images index))]
           (for [thumbnail (-> viewer :thumbnails)]
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
           (-> store :context) %))))))

(defn store-double-state [store]
  (let [viewer (-> store :context :viewer)]
    (when-let [images (-> viewer :images)]
      (let [index (-> viewer :index)
            count (count images)
            folder-id (-> store :context :folder-id)]
        (double-image-viewer-state/state
         :size
         (-> viewer :size)

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
           (for [thumbnail (-> viewer :thumbnails)]
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
           (-> store :context) %))))))
