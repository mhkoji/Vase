(ns vase.controller.edit-folder-tags
  (:require [cljs.core.async :refer [go <!]]
            [vase.entity.tag :as tag]
            [vase.entity.tag-edit :as tag-edit]
            [vase.use-case.edit-folder-tags
             :as edit-folder-tags-use-case]
            [vase.components.tag_editing.state
             :as tag-editing-state]))

(defn create-store [update-store!]
  {:db
   {:folder-id nil
    :tag (tag/repository. [])
    :tag-edit (tag-edit/repository. nil)
    :new-tag-name ""}
   :update-db
   {:folder-id
    #(update-store! (fn [s] (update-in s [:db :folder-id] %)))
    :tag
    #(update-store! (fn [s] (update-in s [:db :tag] %)))
    :tag-edit
    #(update-store! (fn [s] (update-in s [:db :tag-edit] %)))
    :new-tag-name
    #(update-store! (fn [s] (update-in s [:db :new-tag-name] %)))}})

(defn submit-tag [update-tag! update-new-tag-name! new-tag-name]
  (edit-folder-tags-use-case/add-tag update-tag! new-tag-name)
  (update-new-tag-name! (fn [_] "")))

(defn tag-attached-p [tag-edit tag]
  (let [tag-id (-> tag :tag-id)
        attached-ids (tag-edit/list-attached-ids tag-edit)]
    (not (empty? (filter #(= % tag-id) attached-ids)))))


(defn store-state [store]
  (when-let [tag-edit (-> store :db :tag-edit tag-edit/find-edit)]
    (tag-editing-state/state
     :new-tag
     (tag-editing-state/new-tag-state
      :name (-> store :db :new-tag-name)
      :on-submit #(submit-tag (-> store :update-db :tag)
                              (-> store :update-db :new-tag-name)
                              (-> store :db :new-tag-name))
      :on-change #((-> store :update-db :new-tag-name) (fn [_] %)))

     :attach-list
     (for [tag (tag/find-all (-> store :db :tag))]
       (let [attached-p (tag-attached-p tag-edit tag)]
         (tag-editing-state/attach-state
          :tag tag :attached-p attached-p
          :on-delete #(edit-folder-tags-use-case/delete-tag
                       (-> store :update-db :tag)
                       (-> store :update-db :tag-edit)
                       (-> tag :tag-id))
          :on-toggle #(if attached-p
                        (edit-folder-tags-use-case/detach-tag
                         (-> store :update-db :tag-edit)
                         (-> tag :tag-id))
                        (edit-folder-tags-use-case/attach-tag
                         (-> store :update-db :tag-edit)
                         (-> tag :tag-id))))))

     :on-cancel
     #(edit-folder-tags-use-case/cancel-edit
       (-> store :update-db :tag-edit))

     :on-submit
     #(edit-folder-tags-use-case/submit-edit
       (-> store :update-db :tag-edit)
       (-> store :db :folder-id)
       (-> store :db :tag-edit)))))


(defn store-start-edit [store folder-id]
  (edit-folder-tags-use-case/start-edit
   (-> store :update-db :folder-id)
   (-> store :update-db :tag)
   (-> store :update-db :tag-edit)
   folder-id))
