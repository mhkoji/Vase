(ns vase.use-cases.show-tagged-folders
  (:require [vase.api.folder :as folder-api]
            [vase.api.tag :as tag-api]
            [vase.entities.tag]
            [vase.entities.tag-name-edit]
            [vase.entities.transaction :refer [state update-state]]
            [vase.use-cases.show-folders]))

(defrecord State [tag-id tag-db folders tag-name-edit])

(defn show-tags [transaction]
  (tag-api/list-tags (fn [tags]
    (update-state transaction
      #(assoc % :tag-db (vase.entities.tag/Db. tags))))))

(defn show-folders [transaction]
  (update-state transaction #(assoc % :folders nil))
  (let [k (fn [folders]
            (vase.use-cases.show-folders/call-partitioned folders
             (fn [sub]
               (update-state transaction
                 #(update % :folders concat sub)))))]
    (if-let [tag-id (-> transaction state :tag-id)]
      (folder-api/list-by-tag tag-id k)
      (folder-api/list-by-range 0 100 k))))

(defn start-name-edit [transaction]
  (let [state (state transaction)]
    (when-let [tag (vase.entities.tag/load-by-id
                    (-> state :tag-db)
                    (-> state :tag-id))]
      (update-state transaction
        #(assoc % :tag-name-edit (vase.entities.tag-name-edit/TagNameEdit.
                                  (-> tag :tag-id)
                                  (-> tag :name)))))))

(defn submit-name [transaction]
  (when-let [edit (-> transaction state :tag-name-edit)]
    (tag-api/set-name (-> edit :tag-id) (-> edit :name) (fn []
      (show-tags transaction)
      (update-state transaction #(assoc % :tag-name-edit nil))))))

(defn cancel-name-edit [transaction]
  (update-state transaction #(assoc % :tag-name-edit nil)))

(defn change-name [transaction name]
  (update-state transaction #(assoc-in % [:tag-name-edit :name] name)))
