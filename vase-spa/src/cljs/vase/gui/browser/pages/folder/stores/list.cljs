(ns vase.gui.browser.pages.folder.stores.list
  (:require [vase.use-cases.show-folder-details]
            [vase.use-cases.edit-folder-tags]
            [vase.gui.controllers.edit-folder-tags]
            [vase.gui.components.header.state :as header-state]
            [vase.gui.browser.url :as url]))

(defn create-store [update-store folder-id images]
  {:context
   {:show-folder-details
    (vase.use-cases.show-folder-details/OverviewContext.
     #(update-store
       (fn [s] (update-in s [:context :show-folder-details] %)))
     folder-id
     (vase.use-cases.show-folder-details/Overview. nil images))
    :edit-folder-tags
    (vase.use-cases.edit-folder-tags/State. nil nil
     #(update-store
       (fn [s] (update-in s [:context :edit-folder-tags] %))))}})

(defn image->thumbnail-state [folder-id image]
  (let [{:keys [image-id url]} image]
    {:id image-id
     :link (url/read-folder-by-single folder-id image-id)
     :thumbnail-url url}))

(defn store-header-state [store]
  (header-state/get-state :folder))

(defn store-body-state [store]
  (let [overview (-> store :context :show-folder-details :overview)]
    {:title
     (or (-> overview :name) "Folder")
     :tag
     (vase.gui.controllers.edit-folder-tags/context-state
      (-> store :context :edit-folder-tags))
     :thumbnails
     (when-let [images (-> overview :images)]
       (map (partial image->thumbnail-state
                     (-> store :context :show-folder-details :folder-id)
            images)))}))

(defn store-start-edit-folder-tags [store]
  (vase.use-cases.edit-folder-tags/start
   (-> store :context :edit-folder-tags)
   (-> store :context :show-folder-details :folder-id)))

(defn store-show-folders [store]
  (vase.use-cases.show-folder-details/show-overview
   (-> store :context :edit-folder-tags)))
