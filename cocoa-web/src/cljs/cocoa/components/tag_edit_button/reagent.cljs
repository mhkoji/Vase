(ns cocoa.components.tag-edit-button.reagent)

(defn tag-edit-button [{:keys [on-edit]}]
  [:button {:type "button" :class "btn" :on-click on-edit}
   "Tags"])
