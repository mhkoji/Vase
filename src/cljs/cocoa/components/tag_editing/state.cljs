(ns cocoa.components.tag_editing.state)

(defn new-tag-state [& {:keys [name on-submit on-change]}]
  {:name name :on-add on-submit :on-change on-change})

(defn attach-state [& {:keys [tag attached-p on-delete on-toggle]}]
  {:id (-> tag :tag-id)
   :name (-> tag :name)
   :attached-p attached-p
   :on-delete on-delete
   :on-toggle on-toggle})

(defn state [& {:keys [new-tag attach-list on-cancel on-submit]}]
  {:new new-tag
   :list attach-list
   :on-cancel on-cancel
   :on-submit on-submit})
