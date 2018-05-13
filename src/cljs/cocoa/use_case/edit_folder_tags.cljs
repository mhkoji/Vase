(ns cocoa.use-case.edit-folder-tags
  (:require [cljs.core.async :refer [go <! timeout]]
            [cocoa.infra.api.tag :as tag-api]
            [cocoa.infra.api.folder :as folder-api]
            [cocoa.entity.tag :as tag]
            [cocoa.entity.tag-edit :as tag-edit]))

(defn start-edit [update-folder-id!
                  update-tag!
                  update-tag-edit!
                  folder-id]
  (update-folder-id! (fn [_]
    folder-id))
  (go (let [tags (<! (tag-api/list-tags))]
        (update-tag! (fn [repos]
          (-> repos (tag/delete-all) (tag/add-all tags))))))
  (go (let [tags (<! (folder-api/list-tags folder-id))
            tag-edit (tag-edit/edit. (map :tag-id tags))]
        (update-tag-edit! (fn [repos]
          (tag-edit/save-edit repos tag-edit))))))

(defn submit-edit [update-tag-edit! folder-id tag-edit-repos]
  (go (<! (let [tag-edit (tag-edit/find-edit tag-edit-repos)
                attached-ids (tag-edit/list-attached-ids tag-edit)]
            (folder-api/post-tags folder-id attached-ids)))
      (<! (timeout 1000))
      (update-tag-edit! (fn [repos]
        (tag-edit/save-edit repos nil)))))

(defn cancel-edit [update-tag-edit!]
  (update-tag-edit! (fn [repos]
    (tag-edit/save-edit repos nil))))

(defn attach-tag [update-tag-edit! tag-id]
  (update-tag-edit! (fn [repos]
    (let [tag-edit (tag-edit/find-edit repos)
          new-tag-edit (tag-edit/attach tag-edit tag-id)]
      (tag-edit/save-edit repos new-tag-edit)))))

(defn detach-tag [update-tag-edit! tag-id]
  (update-tag-edit! (fn [repos]
    (let [tag-edit (tag-edit/find-edit repos)
          new-tag-edit (tag-edit/detach tag-edit tag-id)]
      (tag-edit/save-edit repos new-tag-edit)))))

(defn add-tag [update-tag! name]
  (go (<! (tag-api/add-tag name))
      (let [tags (<! (tag-api/list-tags))]
        (update-tag! (fn [repos]
          (-> repos (tag/delete-all) (tag/add-all tags)))))))

(defn delete-tag [update-tag!
                  update-tag-edit!
                  tag-id]
  (detach-tag update-tag-edit! tag-id)
  (go (<! (tag-api/delete-tag tag-id))
      (let [tags (<! (tag-api/list-tags))]
        (update-tag! (fn [repos]
          (-> repos (tag/delete-all) (tag/add-all tags)))))))
