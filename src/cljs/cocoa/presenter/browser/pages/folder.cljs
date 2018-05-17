(ns cocoa.presenter.browser.pages.folder
  (:require [goog.events :as gevents]
            [reagent.core :as r]
            [cocoa.components.header.reagent
             :as reagent-header]
            [cocoa.components.viewer.single-image.reagent
             :refer [single-image-viewer]]
            [cocoa.components.viewer.double-image.reagent
             :refer [double-image-viewer]]
            [cocoa.components.tag-edit-button.reagent
             :refer [tag-edit-button]]
            [cocoa.components.tag_editing.reagent
             :refer [modal-editing-tag]]))

(defn list-page [{:keys [header body load-folder edit-folder-tags]}]
  (r/create-class
   {:component-did-mount
    (fn [this]
      (load-folder))

    :reagent-render
    (fn [{:keys [body]}]
      [:div
       [reagent-header/header header]
       (let [{:keys [title tag thumbnails]} body]
         [:main {:class "pt-3 px-4"}
          [:h1 {:class "h2"} title]
          (when tag [modal-editing-tag tag])
          (if (not thumbnails)
            [:div "Loading..."]
            [:container
             [:div
              [:p [tag-edit-button {:on-edit edit-folder-tags}]]]
             [:div {:class "row"}
              (for [thumb thumbnails]
                ^{:key (-> thumb :id)}
                [:div {:class "col-md-4"}
                 [:div {:class "card mb-4 box-shadow"}
                  [:a {:href (-> thumb :link)}
                   [:img {:class "card-img-top"
                          :src (-> thumb :thumbnail-url)}]]]])]])])])}))


(defn generic-viewer-page [{:keys [header render resize load-images]}]
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
      (fn [{:keys [header render]}]
        (cond header
              [:div
               [reagent-header/header header]
               [:main {:class "pt-3 px-4"}
                [:h1 {:class "h2"} "Folder"]
                [:div "Loading..."]]]
              render
              (render)))})))

(defn double-page [{:keys [header viewer resize load-images]}]
  [generic-viewer-page {:header header
                        :resize resize
                        :load-images load-images
                        :render (when viewer
                                  (fn [_]
                                    [double-image-viewer viewer]))}])

(defn single-page [{:keys [header resize viewer load-images]}]
  [generic-viewer-page {:header header
                        :resize resize
                        :load-images load-images
                        :render (when viewer
                                  (fn [_]
                                    [single-image-viewer viewer]))}])
