(ns vase.gui.browser.pages.folders.view
  (:require [goog.Uri :as guri]
            [reagent.core :as r]
            [cljs.reader :refer [read-string]]
            [vase.use-cases.show-folders]
            [vase.gui.components.header.reagent
             :refer [header]]
            [vase.gui.components.nav.reagent
             :refer [pager]]
            [vase.gui.components.folder.reagent
             :refer [cards]]
            [vase.gui.components.tag_editing.reagent
             :refer [modal-editing-tag]]))

(defn loading []
  [:div "Loading..."])

(defn page [{:keys [header-state tag-state body-state]}]
  (r/create-class
   {:component-did-mount
    (fn [comp]
      (let [search (.-search js/location)
            query-data (guri/QueryData.
                        (if (= search "") "" (subs search 1)))]
        ((-> body-state :on-show-folders)
         (read-string (.get query-data "from" "0"))
         (read-string (.get query-data "size" "500")))))

    :reagent-render
    (fn [{:keys [header-state tag-state body-state]}]
      [:div
       ;; header
       [header header-state]
       [:main {:class "pt-3 px-4"}
        [:h1 {:class "h2"} "Folders"]
        ;; modal (in document)
        (when tag-state
          [modal-editing-tag tag-state])
        ;; folders
        (if (-> body-state :folders)
          [:main {:class "pt-3 px-4"}
           [pager (-> body-state :nav)]
           [cards (-> body-state :folders)]
           [pager (-> body-state :nav)]]
          [loading])]])}))
