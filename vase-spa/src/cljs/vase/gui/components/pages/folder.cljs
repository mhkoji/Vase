(ns vase.gui.components.pages.folder
  (:require [goog.Uri :as guri]
            [goog.events :as gevents]
            [reagent.core :as r]
            [cljs.reader :refer [read-string]]
            [vase.use-cases.show-folders]
            [vase.gui.components.header.state
             :as header-state]
            [vase.gui.components.header.reagent
             :refer [header]]
            [vase.gui.components.nav.reagent
             :refer [pager]]
            [vase.gui.components.folder.reagent
             :refer [cards]]
            [vase.gui.components.tag-edit-button.reagent
             :refer [tag-edit-button]]
            [vase.gui.components.tag-editing.reagent
             :refer [modal-editing-tag]]))

(defn list-page [{:keys [title
                         tag-edit on-start-tag-edit
                         thumbnails]}]
  [:div
   [header (header-state/get-state :folder)]
   [:main {:class "pt-3 px-4"}
    [:h1 {:class "h2"} title]
    (when tag-edit
      [modal-editing-tag tag-edit])
    (if (not thumbnails)
      [:div "Loading..."]
      [:container
       [:div
        [:p [tag-edit-button {:on-edit on-start-tag-edit}]]]
       [:div {:class "row"}
        (for [thumb thumbnails]
          ^{:key (-> thumb :id)}
          [:div {:class "col-md-4"}
           [:div {:class "card mb-4 box-shadow"}
            [:a {:href (-> thumb :link)}
             [:img {:class "card-img-top"
                    :src (-> thumb :thumbnail-url)}]]]])]])]])

(defn viewer-page [{:keys [on-resize-window]}]
  (letfn [(resize []
            (on-resize-window
             {:width (.-innerWidth js/window)
              :height (.-innerHeight js/window)}))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (.addEventListener js/window gevents/EventType.RESIZE resize)
        (resize))
      :component-will-unmount
      (fn [this]
        (.removeEventListener js/window gevents/EventType.RESIZE resize))

      :reagent-render
      (fn [{:keys [header render]}]
        (cond header
              [:div
               (header)
               [:main {:class "pt-3 px-4"}
                [:h1 {:class "h2"} "Folder"]
                [:div "Loading..."]]]

              render
              (render)))})))
