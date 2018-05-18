(ns cocoa.presenter.browser.pages.folder
  (:require [goog.events :as gevents]
            [goog.Uri :as guri]
            [goog.History]
            [reagent.core :as r]

            [cocoa.controller.read-folder.by-list
             :as by-list]
            [cocoa.controller.read-folder.by-double-image-viewer
             :as by-double]
            [cocoa.controller.read-folder.by-single-image-viewer
             :as by-single]

            [cocoa.components.header.reagent
             :refer [header]]
            [cocoa.components.tag-edit-button.reagent
             :refer [tag-edit-button]]
            [cocoa.components.tag_editing.reagent
             :refer [modal-editing-tag]]
            [cocoa.components.viewer.single-image.reagent
             :refer [single-image-viewer]]
            [cocoa.components.viewer.double-image.reagent
             :refer [double-image-viewer]]
            [cocoa.presenter.browser.util :refer [render-iter]]))


(defn list-page [{:keys [store]}]
  [:div
   [header (by-list/store-header-state store)]
   (let [{:keys [title tag thumbnails]} (by-list/store-body-state store)]
     [:main {:class "pt-3 px-4"}
      [:h1 {:class "h2"} title]
      (when tag
        [modal-editing-tag tag])
      (if (not thumbnails)
        [:div "Loading..."]
        [:container
         [:div
          [:p [tag-edit-button {:on-edit
                                #(by-list/store-edit-folder-tags! store)}]]]
         [:div {:class "row"}
          (for [thumb thumbnails]
            ^{:key (-> thumb :id)}
            [:div {:class "col-md-4"}
             [:div {:class "card mb-4 box-shadow"}
              [:a {:href (-> thumb :link)}
               [:img {:class "card-img-top"
                      :src (-> thumb :thumbnail-url)}]]]])]])])])


(defn generic-viewer-page [{:keys [resize-window]}]
  (r/create-class
   {:component-did-mount
    (fn [this]
      (.addEventListener js/window gevents/EventType.RESIZE
                         resize-window)
      (resize-window))
    :component-will-unmount
    (fn [this]
      (.removeEventListener js/window gevents/EventType.RESIZE
                            resize-window))
    :reagent-render
    (fn [{:keys [header render]}]
      (cond header
            [:div
             (header)
             [:main {:class "pt-3 px-4"}
              [:h1 {:class "h2"} "Folder"]
              [:div "Loading..."]]]
            render
            (render)))}))

(defn double-page [{:keys [store]}]
  [generic-viewer-page
   {:resize-window
    (fn []
      (by-double/store-resize! store
                               (.-innerWidth js/window)
                               (.-innerHeight js/window)))
    :header
    (when-let [header-state (by-double/store-header-state store)]
      (fn [] [header header-state]))
    :render
    (when-let [viewer-state (by-double/store-viewer-state store)]
      (fn [] [double-image-viewer viewer-state]))}])

(defn single-page [{:keys [store]}]
  [generic-viewer-page
   {:resize-window
    (fn []
      (by-single/store-resize! store
                               (.-innerWidth js/window)
                               (.-innerHeight js/window)))
    :header
    (when-let [header-state (by-single/store-header-state store)]
      (fn [] [header header-state]))
    :render
    (when-let [viewer-state (by-single/store-viewer-state store)]
      (fn [] [single-image-viewer viewer-state]))}])


(defn get-uri []
  (goog.Uri.
   (let [hash (.-hash js/location)]
     (if (= hash "") "" (subs hash 1)))))

(defn show-internal [elem folder-id]
  (render-iter
   (let [uri (get-uri)]
     (cond (= (.getPath uri) "spread")
           {:create #(let [store (by-double/create-store % folder-id nil)]
                       (by-double/store-load-images! store)
                       store)
            :render #(r/render [double-page {:store %}] elem)}
           (= (.getPath uri) "single")
           {:create #(let [store (by-single/create-store % folder-id nil)]
                       (by-single/store-load-images! store)
                       store)
            :render #(r/render [single-page {:store %}] elem)}
           :else
           {:create #(let [store (by-list/create-store % folder-id nil)]
                       (by-list/store-load-folder! store)
                       store)
            :render #(r/render [list-page {:store %}] elem)}))))

(defn show [elem {:keys [folder-id]}]
  (let [history (goog.History.)]
    (gevents/listen history
                    goog.History/EventType.NAVIGATE
                    #(show-internal elem folder-id))
    (.setEnabled history true))
  (show-internal elem folder-id))
