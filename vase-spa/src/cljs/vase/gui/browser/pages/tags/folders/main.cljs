(ns vase.gui.pages.tags.folders.main
  (:require [reagent.core :as r]
            [vase.gui.pages.tags.folders.view.store :as store]
            [vase.gui.components.header.reagent :refer [header]]
            [vase.gui.components.folder.reagent :refer [cards]]
            [vase.gui.browser.util :refer [render-iter]]))

(defn page [{:keys [store]}]
  [:div
   [header (store/store-header-state store)]

   (let [body (store/store-body-state store)]
     [:main {:class "pt-3 px-4"}
      [:h1 {:class "h2"} (-> body :title)]

      [:div
       (let [navs (-> body :navs)]
         [:ul {:class "nav nav-tabs"}
          (for [nav navs]
            (let [{:keys [id active-p name url]} nav]
              ^{:key id}
              [:li {:class "nav-item"}
               [:a {:class (str "nav-link" (if active-p " active" ""))
                    :href url}
                name]]))])

       [:div {:class "pt-3"}
        (when-let [tag-name-editing (-> body :tag-name-editing)]
          [:dialog
           [:div {:class "input-group"}
            [:input {:type "text"
                     :class "form-control"
                     :value (-> tag-name-editing :value)
                     :on-change #((-> tag-name-editing :on-change)
                                  (.-value (.-target %)))}]
            [:div {:class "input-group-append"}
             [:button {:class "btn btn-outline-secondary"
                       :on-click (-> tag-name-editing :on-cancel)}
              [:span {:class "oi oi-circle-x" :aria-hidden "true"}]]
             [:button {:class "btn btn-outline-secondary btn-primary"
                       :on-click (-> tag-name-editing :on-submit)}
              [:span {:class "oi oi-cloud-upload" :aria-hidden "true"}]]]]])

        (when-let [ops (-> body :ops)]
          [:div {:class "btn-toolbar mb-2 mb-md-0"}
           [:div {:class "btn-group mr-2"}
            [:button {:class "btn btn-default btn-sm"
                      :on-click (-> ops :on-edit)}
             [:span {:class "oi oi-pencil" :aria-hidden "true"}]]]
           [:div {:class "btn-group mr-2"}
            [:button {:class "btn btn-danger btn-sm"
                      :on-click (-> ops :on-delete)}
             [:span {:class "oi oi-trash" :aria-hidden "true"}]]]])

        (when (not (-> body :tag-name-editing))
          (if-let [folders (-> body :folders)]
            [cards folders]
            [:div "Loading..."]))]]])])

(defn show [elem {:keys [tag-id]}]
  (render-iter
   {:create #(let [store (store/create-store % tag-id)]
               (store/store-show-tags store)
               (store/store-show-tagged-folders store)
               store)
    :render #(r/render [page {:store %}] elem)}))
