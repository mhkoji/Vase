(ns cocoa.controller.tag_editing.reagent
  (:require [reagent.core :as r]
            [cljsjs.react-modal]))

(defn modal-editing-tag [{:keys [new list on-cancel on-submit]}]
  (r/create-element
   js/ReactModal
   #js {:isOpen true
        :contentLabel "Tags"
        :onRequestClose on-cancel}
   (r/as-element
    [:div
     [:div
      [:div {:class "input-group"}
       [:input {:type "text" :class "form-control" :value (-> new :name)
                :on-change #((-> new :on-change)
                             (.-value (.-target %)))}]
       [:div {:class "input-group-append"}
        [:button {:type "button" :class"btn btn-primary"
                  :on-click #((-> new :on-add))}
         [:span {:class "oi oi-plus"}]]]]
      [:ul {:class "list-group"}
       (for [item list]
         (let [{:keys [id name attached-p on-toggle on-delete]} item]
           ^{:key id}
           [:li {:class "list-group-item"}
            [:label
             [:input {:type "checkbox" :checked attached-p
                      :on-change on-toggle}]
             name]
            [:div {:class "float-right"}
             [:button {:type "button" :class "btn btn-danger btn-sm"
                       :on-click on-delete}
              [:span {:class "oi oi-delete"}]]]]))]]

     [:div {:class "modal-footer"}
      [:div {:class "form-row align-items-center"}
       [:div {:class "col-auto"}
        [:button {:on-click on-cancel :class "btn"}
         "Cancel"]]
       [:div {:class "col-auto"}
        [:button {:on-click on-submit :class "btn btn-primary"}
         "Save"]]]]])))
