(ns cocoa.presenter.browser.pages.folders
  (:require [reagent.core :as r]
            [cocoa.controller.header.reagent :as reagent-header]
            [cocoa.controller.nav.reagent :refer [pager]]
            [cocoa.controller.folder.reagent :refer [cards]]
            [cocoa.controller.tag_editing.reagent
             :refer [modal-editing-tag]]))

(defn loading []
  [:div "Loading..."])

(defn page [{:keys [header tag body load-folders]}]
  (r/create-class
   {:component-did-mount
    (fn [this]
      (load-folders))

    :reagent-render
    (fn [{:keys [tag body]}]
      [:div
       ;; header
       [reagent-header/header header]
       [:main {:class "pt-3 px-4"}
        [:h1 {:class "h2"} "Folders"]
        ;; modal (in document)
        (when tag [modal-editing-tag tag])
        ;; folders
        (if body
          [:main {:class "pt-3 px-4"}
           [pager (-> body :nav)]
           [cards (-> body :folders)]
           [pager (-> body :nav)]]
          [loading])]])}))
