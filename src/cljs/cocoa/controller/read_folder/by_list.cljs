(ns cocoa.controller.read-folder.by-list
  (:require [cljs.core.async :refer [go <!]]
            [cocoa.entity.folder :as folder]
            [cocoa.use-case.load-folder :as load-folder]
            [cocoa.components.header.state :as header-state]

            [cocoa.presenter.browser.url :as url]))

(defn create-store [update-store! folder-id images]
  {:const
   {:folder-id folder-id}
   :db
   {:folder (folder/save-folder
             (folder/repository. [])
             {:folder-id folder-id :images images})}
   :update-db
   {:folder
    #(update-store! (fn [s] (update-in s [:db :folder] %)))}})

(defn image->thumbnail-state [folder-id image]
  (let [{:keys [image-id url]} image]
    {:id image-id
     :link (url/read-folder-by-spread folder-id image-id)
     :thumbnail-url url}))

(defn store-state [store]
  (let [folder-repos (-> store :db :folder)
        folder-id    (-> store :const :folder-id)
        folder       (folder/find-folder-by-id folder-repos folder-id)]
    {:header
     (header-state/get-state :folder)
     :body
     {:title
      (or (-> folder :name) "Folder")
      :thumbnails
      (if-let [images (-> folder :images)]
        (map (partial image->thumbnail-state folder-id) images)
        nil)}
     :load-folder
     #(load-folder/load-folder folder-repos
                               (-> store :update-db :folder)
                               folder-id)}))
