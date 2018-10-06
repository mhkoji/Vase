(ns vase.controller.read-folder.by-list
  (:require [cljs.core.async :refer [go <!]]
            [vase.entity.folder :as folder]
            [vase.use-case.load-folder :as load-folder]
            [vase.components.header.state :as header-state]
            [vase.controller.edit-folder-tags :as edit-folder-tags]

            [vase.presenter.browser.url :as url]))

(defn create-store [update-store! folder-id images]
  {:const
   {:folder-id folder-id}
   :db
   {:folder (folder/save-folder
             (folder/repository. [])
             {:folder-id folder-id :images images})}
   :substore
   {:edit-folder-tags
    (edit-folder-tags/create-store
     #(update-store!
       (fn [s] (update-in s [:substore :edit-folder-tags] %))))}
   :update-db
   {:folder
    #(update-store! (fn [s] (update-in s [:db :folder] %)))}})

(defn image->thumbnail-state [folder-id image]
  (let [{:keys [image-id url]} image]
    {:id image-id
     :link (url/read-folder-by-spread folder-id image-id)
     :thumbnail-url url}))

(defn store-header-state [store]
  (header-state/get-state :folder))

(defn store-body-state [store]
  (let [folder-repos (-> store :db :folder)
        folder-id    (-> store :const :folder-id)
        folder       (folder/find-folder-by-id folder-repos folder-id)]
    {:title (or (-> folder :name) "Folder")
     :tag (edit-folder-tags/store-state
           (-> store :substore :edit-folder-tags))
     :thumbnails (if-let [images (-> folder :images)]
                   (map (partial image->thumbnail-state folder-id) images)
                   nil)}))

(defn store-edit-folder-tags! [store]
  (edit-folder-tags/store-start-edit (-> store :substore :edit-folder-tags)
                                     (-> store :const :folder-id)))

(defn store-load-folder! [store]
  (load-folder/load-folder (-> store :db :folder)
                           (-> store :update-db :folder)
                           (-> store :const :folder-id)))
