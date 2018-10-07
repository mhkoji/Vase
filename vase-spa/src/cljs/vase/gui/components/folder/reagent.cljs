(ns vase.gui.components.folder.reagent
  (:require [vase.gui.browser.url :as url]
            [vase.gui.components.tag-edit-button.reagent
             :refer [tag-edit-button]]))

(defn card [{:keys [folder-id name thumbnail-url on-edit-tag]}]
  (let [link (url/folder folder-id)]
    [:div {:class "card mb-4 box-shadow" :style {:maxWidth "18rem"}}
     [:a {:href link}
      [:img {:src thumbnail-url :class "card-img-top"}]]

     [:div {:class "card-body"}
      [:div {:class "card-title"} name]
      (when on-edit-tag
        [:p [tag-edit-button {:on-edit on-edit-tag}]])]]))

(defn rows [num% items item-key item-render]
  (let [count (count items)]
    (let [num (min num% count)
          group (group-by first
                 (map-indexed (fn [index item]
                                (list (quot index num) item))
                              items))]
      [:div {:class "container"}
       (for [row-index (sort < (keys group))]
         ^{:key (str row-index)}
         [:div {:class "card-deck"}
          (for [[_ item] (group row-index)]
            ^{:key (item-key item)}
            [item-render item])])])))

(defn cards [folders]
  [rows 4 folders :folder-id card])
