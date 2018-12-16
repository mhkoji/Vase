(ns vase.gui.browser.pages.folder.drivers.list
  (:require [vase.entities.transaction :refer [state]]
            [vase.use-cases.show-folder-details]
            [vase.use-cases.edit-folder-tags]
            [vase.gui.controllers.edit-folder-tags]
            [vase.gui.components.header.state :as header-state]
            [vase.gui.components.pages.folder :as folder-page]
            [vase.gui.browser.url :as url]
            [reagent.core :as r]))

(defn create-store [update-store folder-id]
  {:show-folder-details
   (vase.entities.transaction/DelegateTransaction.
    (vase.use-cases.show-folder-details/OverviewState. folder-id nil nil)
    #(update-store
      (fn [s] (update-in s [:show-folder-details :state] %))))

   :edit-folder-tags
   (vase.entities.transaction/DelegateTransaction.
    (vase.use-cases.edit-folder-tags/State. nil nil nil)
    #(update-store
      (fn [s] (update-in s [:edit-folder-tags :state] %))))})


(defn create [elem folder-id]
  {:create-store
   (fn [update-store]
     (let [store (create-store update-store folder-id)]
       (vase.use-cases.show-folder-details/show-overview
        (-> store :show-folder-details))
       store))
   :render
   (fn [store]
     (js/console.log (str (-> store :show-folder-details state)))
     (r/render
      [folder-page/list-page
       {:title
        (or (-> store :show-folder-details state :name) "Folder")
        :tag-edit
        (vase.gui.controllers.edit-folder-tags/context-state
         (-> store :edit-folder-tags))
        :on-start-tag-edit
        #(vase.use-cases.edit-folder-tags/start
          (-> store :edit-folder-tags)
          folder-id)
        :thumbnails
        (when-let [images (-> store :show-folder-details state :images)]
          (map (fn [x]
                 (let [{:keys [image-id url]} x
                       link (url/read-folder-by-single folder-id image-id)]
                   {:id image-id :link link :thumbnail-url url}))
               images))}]
      elem))})
