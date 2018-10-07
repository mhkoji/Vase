(ns vase.gui.browser.pages.folder.main
  (:require [goog.events :as gevents]
            [goog.Uri :as guri]
            [goog.History]
            [reagent.core :as r]

            [vase.gui.browser.pages.folder.stores.viewer
             :as viewer-store]
            [vase.gui.browser.pages.folder.stores.list
             :as list-store]

            [vase.gui.components.header.reagent
             :refer [header]]
            [vase.gui.components.tag-edit-button.reagent
             :refer [tag-edit-button]]
            [vase.gui.components.tag_editing.reagent
             :refer [modal-editing-tag]]
            [vase.gui.components.viewer.single-image.reagent
             :refer [single-image-viewer]]
            [vase.gui.components.viewer.double-image.reagent
             :refer [double-image-viewer]]
            [vase.gui.browser.util :refer [render-iter]]))


(defn list-page [{:keys [store]}]
  [:div
   [header (list-store/store-header-state store)]
   (let [{:keys [title tag thumbnails]} (list-store/store-body-state store)]
     [:main {:class "pt-3 px-4"}
      [:h1 {:class "h2"} title]
      (when tag
        [modal-editing-tag tag])
      (if (not thumbnails)
        [:div "Loading..."]
        [:container
         [:div
          [:p [tag-edit-button
               {:on-edit
                #(list-store/store-start-edit-folder-tags store)}]]]
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
      (viewer-store/store-resize store
                                 (.-innerWidth js/window)
                                 (.-innerHeight js/window)))
    :header
    (when-let [header-state (viewer-store/store-header-state store)]
      (fn [] [header header-state]))
    :render
    (when-let [viewer-state (viewer-store/store-double-state store)]
      (fn [] [double-image-viewer viewer-state]))}])

(defn single-page [{:keys [store]}]
  [generic-viewer-page
   {:resize-window
    (fn []
      (viewer-store/store-resize store
                                 (.-innerWidth js/window)
                                 (.-innerHeight js/window)))
    :header
    (when-let [header-state (viewer-store/store-header-state store)]
      (fn [] [header header-state]))
    :render
    (when-let [viewer-state (viewer-store/store-single-state store)]
      (fn [] [single-image-viewer viewer-state]))}])


(defn get-uri []
  (goog.Uri.
   (let [hash (.-hash js/location)]
     (if (= hash "") "" (subs hash 1)))))

(defn show-internal [elem folder-id]
  (render-iter
   (let [uri (get-uri)]
     (cond (= (.getPath uri) "spread")
           {:create
            #(let [store (viewer-store/create-store % folder-id nil)]
               (viewer-store/store-show-viewer store)
               store)
            :render
            #(r/render [double-page {:store %}] elem)}
           (= (.getPath uri) "single")
           {:create
            #(let [store (viewer-store/create-store % folder-id nil)]
               (viewer-store/store-show-viewer store)
               store)
            :render
            #(r/render [single-page {:store %}] elem)}
           :else
           {:create
            #(let [store (list-store/create-store % folder-id nil)]
               (list-store/store-show-folders store)
               store)
            :render
            #(r/render [list-page {:store %}] elem)}))))

(defn show [elem {:keys [folder-id]}]
  (let [history (goog.History.)]
    (gevents/listen history
                    goog.History/EventType.NAVIGATE
                    #(show-internal elem folder-id))
    (.setEnabled history true))
  (show-internal elem folder-id))
