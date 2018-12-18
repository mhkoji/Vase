(ns vase.gui.browser.pages.tags.folders
  (:require [vase.entities.tag]
            [vase.entities.transaction :refer [state]]
            [vase.use-cases.show-tagged-folders]
            [vase.gui.components.folder.state :as folder-state]
            [vase.gui.components.pages.tags.folders]
            [vase.gui.browser.url :as url]
            [vase.gui.browser.util]
            [vase.api.tag :as tag-api]
            [reagent.core :as r]))

(defn create-store [update-store tag-id]
  {:show-tagged-folders
   (vase.entities.transaction/DelegateTransaction.
    (vase.use-cases.show-tagged-folders/State. tag-id nil nil nil)
    #(update-store
      (fn [s] (update-in s [:show-tagged-folders :state] %))))})

(defn create-renderer [elem]
  (fn [store]
    (let [transaction (-> store :show-tagged-folders)
          state (-> transaction state)
          tag-id (-> state :tag-id)
          tag-db (-> state :tag-db)
          tag-name-edit (-> state :tag-name-edit)]
      (r/render
       [vase.gui.components.pages.tags.folders/page
        {:tag-name-editing
         (when tag-name-edit
           {:value (:name tag-name-edit)
            :on-change #(vase.use-cases.show-tagged-folders/change-name
                         transaction %)
            :on-cancel #(vase.use-cases.show-tagged-folders/cancel-name-edit
                         transaction)
            :on-submit #(vase.use-cases.show-tagged-folders/submit-name
                         transaction)})
         :navs
         (cons {:id "all"
                :name "All"
                :url (url/tags)
                :active-p (nil? tag-id)}
               (for [tag (vase.entities.tag/load-all tag-db)]
                 {:id (:tag-id tag)
                  :name (:name tag)
                  :url (url/tag-folders (:tag-id tag))
                  :active-p (= (:tag-id tag) tag-id)}))
         :ops
         (when tag-id
           {:on-edit #(vase.use-cases.show-tagged-folders/start-name-edit
                       transaction)
            :on-delete #(tag-api/delete-tag tag-id nil)})

         :folders
         (when-let [folders (-> state :folders)]
           (for [f folders]
             (folder-state/state (-> f :folder-id)
                                 (-> f :name)
                                 (-> f :thumbnail :url)
                                 nil)))}]
       elem))))

(defn render-loop [elem {:keys [tag-id]}]
  (vase.gui.browser.util/render-loop
   {:create-store #(let [store (create-store % tag-id)]
                     (vase.use-cases.show-tagged-folders/show-tags
                      (-> store :show-tagged-folders))
                     (vase.use-cases.show-tagged-folders/show-folders
                      (-> store :show-tagged-folders))
                     store)
    :render (create-renderer elem)}))
