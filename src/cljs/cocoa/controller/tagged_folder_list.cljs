(ns cocoa.controller.tagged-folder-list
  (:require [cljs.core.async :refer [go <!]]

            [cocoa.components.header.state :as header-state]

            [cocoa.entity.folder :as folder]
            [cocoa.use-case.load-folders :as load-folders]
            [cocoa.components.folder.state :as folder-state]

            [cocoa.entity.tag :as tag]
            [cocoa.infra.api.tag :as tag-api]
            [cocoa.use-case.load-tags :as load-tags]

            [cocoa.presenter.browser.url :as url]))

(defn create-store [update-store! tag-id]
  {:const
   {:tag-id tag-id}
   :db
   {:tag (tag/repository. [])
    :folder (folder/repository. nil)
    :name-in-edit nil}
   :update-db
   {:tag
    #(update-store! (fn [s] (update-in s [:db :tag] %)))
    :folder
    #(update-store! (fn [s] (update-in s [:db :folder] %)))
    :name-in-edit
    #(update-store! (fn [s] (update-in s [:db :name-in-edit] %)))}})

(defn store-state [store]
  {:header
   (header-state/get-state :tag)
   :body
   (let [tag (when-let [tag-id (-> store :const :tag-id)]
               (-> store :db :tag (tag/find-tag tag-id)))]
     {:title
      "Tags"
      :tag-name-editing
      (when-let [name (-> store :db :name-in-edit)]
        {:value name
         :on-change #(load-tags/change-name
                      (-> store :update-db :name-in-edit)
                      %)
         :on-cancel #(load-tags/cancel-change
                      (-> store :update-db :name-in-edit))
         :on-submit #(load-tags/submit-name
                      (-> store :update-db :tag)
                      (-> store :update-db :name-in-edit)
                      (:tag-id tag)
                      name)})
      :ops
      (when (and tag (not (-> store :db :name-in-edit)))
        {:on-edit #(load-tags/change-name
                    (-> store :update-db :name-in-edit)
                    (:name tag))
         :on-delete #(tag-api/delete-tag (:tag-id tag))})
      :navs
      (cons {:id "all" :name "All" :url (url/tags) :active-p (nil? tag)}
            (for [tag (-> store :db :tag (tag/find-all))]
              {:id (:tag-id tag)
               :name (:name tag)
               :url (url/tag-folders (:tag-id tag))
               :active-p (= (:tag-id tag) (-> store :const :tag-id))}))
      :folders
      (when-let [folders (-> store :db :folder folder/find-all)]
        (for [f folders]
          (folder-state/state (-> f :folder-id)
                              (-> f :name)
                              (-> f :thumbnail :url)
                              nil)))
      :on-mounted
      (fn []
        (load-tags/load-all (-> store :update-db :tag))
        (load-folders/load-by-tag (-> store :update-db :folder)
                                  (-> store :const :tag-id)))})})
