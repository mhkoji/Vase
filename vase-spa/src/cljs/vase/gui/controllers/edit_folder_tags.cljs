(ns vase.gui.controllers.edit-folder-tags
  (:require [vase.entities.tag]
            [vase.entities.tag-edit]
            [vase.entities.tag-name-edit]
            [vase.entities.transaction]
            [vase.use-cases.edit-folder-tags]
            [vase.gui.components.tag_editing.state]))

(defn context-state [transaction]
  (let [state (-> transaction vase.entities.transaction/state)]
    (when-let [tag-edit (-> state :tag-edit)]
      (vase.gui.components.tag_editing.state/state
       :new-tag
       (vase.gui.components.tag_editing.state/new-tag-state
        :name (:name (-> state :tag-name-edit))
        :on-submit #(vase.use-cases.edit-folder-tags/submit-new-tag
                     transaction)
        :on-change #(vase.use-cases.edit-folder-tags/change-new-tag-name
                     transaction %))

       :attach-list
       (for [tag (vase.entities.tag/load-all (-> state :tag-db))]
         (let [attached-p (vase.entities.tag-edit/attached-tag-p
                           tag-edit tag)]
           (vase.gui.components.tag_editing.state/attach-state
            :tag tag
            :attached-p attached-p
            :on-delete #(vase.use-cases.edit-folder-tags/delete-tag
                         transaction (-> tag :tag-id))
            :on-toggle #(if attached-p
                          (vase.use-cases.edit-folder-tags/detach-tag
                           transaction (-> tag :tag-id))
                          (vase.use-cases.edit-folder-tags/attach-tag
                           transaction (-> tag :tag-id))))))

       :on-cancel #(vase.use-cases.edit-folder-tags/cancel transaction)

       :on-submit #(vase.use-cases.edit-folder-tags/submit transaction)))))
