(ns vase.controller.tagged-folder-list
  (:require [cljs.core.async :refer [go <!]]

            [vase.components.header.state :as header-state]

            [vase.entity.folder :as folder]
            [vase.use-case.load-folders :as load-folders]
            [vase.components.folder.state :as folder-state]

            [vase.entity.tag :as tag]
            [vase.infra.api.tag :as tag-api]
            [vase.use-case.load-tags :as load-tags]

            [vase.presenter.browser.url :as url]))

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


(defn store-header-state [store]
  (header-state/get-state :tag))

(defn store-body-state [store]
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
     :navs
     (cons {:id "all" :name "All" :url (url/tags) :active-p (nil? tag)}
           (for [tag (-> store :db :tag (tag/find-all))]
             {:id (:tag-id tag)
              :name (:name tag)
              :url (url/tag-folders (:tag-id tag))
              :active-p (= (:tag-id tag) (-> store :const :tag-id))}))
     :ops
     (when (and tag (not (-> store :db :name-in-edit)))
       {:on-edit #(load-tags/change-name
                   (-> store :update-db :name-in-edit)
                   (:name tag))
        :on-delete #(tag-api/delete-tag (:tag-id tag))})

     :folders
     (when-let [folders (-> store :db :folder folder/find-all)]
       (for [f folders]
         (folder-state/state (-> f :folder-id)
                             (-> f :name)
                             (-> f :thumbnail :url)
                             nil)))}))


(defn store-load-tags! [store]
  (load-tags/load-all (-> store :update-db :tag)))

(defn store-load-folders! [store]
  (load-folders/load-by-tag (-> store :update-db :folder)
                            (-> store :const :tag-id)))
