(ns vase.use-cases.show-tagged-folders
  (:require [vase.api.folder :as folder-api]
            [vase.api.tag :as tag-api]
            [vase.entities.tag]
            [vase.entities.tag-name-edit]
            [vase.use-cases.show-folders]
            [vase.use-cases.edit-folder-tags]))

(defn update-field [context key fn]
  ((-> context :update-context) #(update % key fn)))

(defrecord Context [update-context tag-id tag-db folders tag-name-edit])

(defn show-tags [context]
  (tag-api/list-tags (fn [tags]
    (update-field context :tag-db #(vase.entities.tag/Db. tags)))))

(defn show-folders [context]
  (update-field context :folders (fn [_] nil))
  (let [k (fn [folders]
            (vase.use-cases.show-folders/call-partitioned folders
              (fn [sub]
                (update-field context :folders #(concat % sub)))))]
    (if-let [tag-id (-> context :tag-id)]
      (folder-api/list-by-tag (-> context :tag-id) k)
      (folder-api/list-by-range 100 100 k))))

(defn start-name-edit [context]
  (when-let [tag (vase.entities.tag/load-by-id (-> context :tag-db)
                                               (-> context :tag-id))]
    (let [name (-> tag :name)]
      (update-field context :tag-name-edit
        #(vase.entities.tag-name-edit/TagNameEdit.
          (-> context :tag-id) name)))))

(defn submit-name [context]
  (when-let [edit (-> context :tag-name-edit)]
    (tag-api/set-name (-> edit :tag-id) (-> edit :name) (fn []
      (show-tags context)
      (update-field context :tag-name-edit (fn [_] nil))))))

(defn cancel-name-edit [context]
  (update-field context :tag-name-edit (fn [_] nil)))

(defn change-name [context name]
  (update-field context :tag-name-edit (fn [edit]
    (when edit
      (assoc edit :name name)))))
