(ns vase.use-cases.edit-folder-tags
  (:require [cljs.core.async :refer [<! go timeout]]
            [vase.api.tag :as tag-api]
            [vase.api.folder :as folder-api]
            [vase.entities.tag]
            [vase.entities.tag-edit]
            [vase.entities.tag-name-edit]
            [vase.entities.transaction :refer [state update-state]]))

(defrecord State [tag-db tag-edit tag-name-edit])

(defn refresh-tags [transaction cont]
  (tag-api/list-tags (fn [tags]
    (update-state transaction
      #(assoc % :tag-db (vase.entities.tag/Db. tags)))
    (when cont (cont)))))

(defn start [transaction folder-id]
  (refresh-tags transaction (fn []
    (folder-api/list-tags folder-id (fn [attached-tags]
      (update-state transaction
        #(assoc % :tag-edit (vase.entities.tag-edit/TagEdit.
                             folder-id
                             attached-tags))))))))

(defn submit [transaction]
  (let [tag-edit (-> transaction state :tag-edit)
        folder-id (-> tag-edit :content)
        attached-tags (-> tag-edit :attached-tags)]
    (folder-api/post-tags folder-id attached-tags (fn []
      (go (<! (timeout 1000))
          (update-state transaction #(assoc % :tag-edit nil)))))))

(defn cancel [transaction]
  (update-state transaction #(assoc % :tag-edit nil)))

(defn attach-tag [transaction tag-id]
  (when-let [tag (vase.entities.tag/load-by-id
                  (-> transaction state :tag-db)
                  tag-id)]
    (update-state transaction
      #(update % :tag-edit vase.entities.tag-edit/attach tag))))

(defn detach-tag [transaction tag-id]
  (when-let [tag (vase.entities.tag/load-by-id
                  (-> transaction state :tag-db)
                  tag-id)]
    (update-state transaction
      #(update % :tag-edit vase.entities.tag-edit/detach tag))))

(defn delete-tag [transaction tag-id]
  (detach-tag transaction tag-id)
  (tag-api/delete-tag tag-id #(refresh-tags transaction nil)))

(defn submit-new-tag [transaction]
  (when-let [edit (-> transaction state :tag-name-edit)]
    (tag-api/add-tag (-> edit :name) (fn []
      (update-state transaction #(assoc % :tag-name-edit nil))
      (refresh-tags transaction nil)))))

(defn change-new-tag-name [transaction name]
  (update-state transaction #(assoc-in % [:tag-name-edit :name] name)))
