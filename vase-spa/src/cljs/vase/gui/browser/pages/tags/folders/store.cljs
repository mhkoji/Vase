(ns vase.gui.pages.tags.folders.view.store
  (:require [vase.api.tag :as tag-api]
            [vase.entities.tag]
            [vase.use-cases.show-tagged-folders]
            [vase.gui.controllers.edit-folder-tags]
            [vase.gui.components.folder.state :as folder-state]
            [vase.gui.components.header.state :as header-state]
            [vase.gui.browser.url :as url]))

(defn create-store [update-store tag-id]
  {:context
   (vase.use-cases.show-tagged-folders/Context.
    #(update-store
      (fn [s] (update-in s [:context :show-tagged-folders] %)))
    tag-id nil nil nil)})

(defn store-header-state [store]
  (header-state/get-state :tag))

(defn store-body-state [store]
  (let [context (-> store :context)
        tag-id (-> store :context :tag-id)
        tag-name-edit (-> context :tag-name-edit)]
    {:title
     "Tags"
     :tag-name-editing
     (when tag-name-edit
       {:value (:name tag-name-edit)
        :on-change #(vase.use-cases.show-tagged-folders/change-name
                     context %)
        :on-cancel #(vase.use-cases.show-tagged-folders/cancel-name-edit
                     context)
        :on-submit #(vase.use-cases.show-tagged-folders/submit-name
                     context)})
     :navs
     (cons {:id "all" :name "All" :url (url/tags) :active-p (nil? tag-id)}
           (for [tag (vase.entities.tag/load-all (-> context :tag-db))]
             {:id (:tag-id tag)
              :name (:name tag)
              :url (url/tag-folders (:tag-id tag))
              :active-p (= (:tag-id tag) tag-id)}))
     :ops
     (when (not tag-name-edit)
       {:on-edit #(vase.use-cases.show-tagged-folders/start-name-edit
                   context)
        :on-delete #(tag-api/delete-tag tag-id nil)})

     :folders
     (when-let [folders (-> context :folders)]
       (for [f folders]
         (folder-state/state (-> f :folder-id)
                             (-> f :name)
                             (-> f :thumbnail :url)
                             nil)))}))

(defn store-show-tags [store]
  (vase.use-cases.show-tagged-folders/show-tags (-> store :context)))

(defn store-show-tagged-folders [store]
  (vase.use-cases.show-tagged-folders/show-folders (-> store :context)))
