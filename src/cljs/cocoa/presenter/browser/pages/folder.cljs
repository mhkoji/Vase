(ns cocoa.presenter.browser.pages.folder
  (:require [goog.events :as gevents]
            [reagent.core :as r]
            [cocoa.components.header.reagent :as reagent-header]
            [cocoa.components.spread-viewer.reagent :as spread-viewer]))

(defn list-page [{:keys [header body load-folder]}]
  (r/create-class
   {:component-did-mount
    (fn [this]
      (load-folder))

    :reagent-render
    (fn [{:keys [body]}]
      [:div
       [reagent-header/header header]
       (let [{:keys [title thumbnails]} body]
         [:main {:class "pt-3 px-4"}
          [:h1 {:class "h2"} title]
          (if (not thumbnails)
            [:div "Loading..."]
            [:container
             [:div {:class "row"}
              (for [thumb thumbnails]
                ^{:key (-> thumb :id)}
                [:div {:class "col-md-4"}
                 [:div {:class "card mb-4 box-shadow"}
                  [:a {:href (-> thumb :link)}
                   [:img {:class "card-img-top"
                          :src (-> thumb :thumbnail-url)}]]]])]])])])}))


(defn spread-page [{:keys [header viewer resize load-images]}]
  (letfn [(resize-window []
            (resize {:width  (.-innerWidth js/window)
                     :height (.-innerHeight js/window)}))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (.addEventListener js/window
                           gevents/EventType.RESIZE
                           resize-window)
        (resize-window)
        (load-images))

      :component-will-unmount
      (fn [this]
        (.removeEventListener js/window
                              gevents/EventType.RESIZE
                              resize-window))

      :reagent-render
      (fn [{:keys [header viewer]}]
        (cond header
              [:div
               [reagent-header/header header]
               [:main {:class "pt-3 px-4"}
                [:h1 {:class "h2"} "Folder"]
                [:div "Loading..."]]]
              viewer
              [spread-viewer/spread-viewer viewer]))})))
