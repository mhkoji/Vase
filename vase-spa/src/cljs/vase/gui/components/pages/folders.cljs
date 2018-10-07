(ns vase.gui.components.pages.folders
  (:require [goog.Uri :as guri]
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
            [vase.gui.components.tag-editing.reagent
             :refer [modal-editing-tag]]))

(defn loading []
  [:div "Loading..."])

(defn page [{:keys [tag-edit folder-show]}]
  (r/create-class
   {:component-did-mount
    (fn [comp]
      (let [search (.-search js/location)
            query-data (guri/QueryData.
                        (if (= search "") "" (subs search 1)))]
        ((-> folder-show :show-folders)
         (read-string (.get query-data "from" "0"))
         (read-string (.get query-data "size" "500")))))

    :reagent-render
    (fn [{:keys [tag-edit folder-show]}]
      [:div
       ;; header
       [header (header-state/get-state :folder)]
       [:main {:class "pt-3 px-4"}
        [:h1 {:class "h2"} "Folders"]
        ;; modal (in document)
        (when tag-edit
          [modal-editing-tag tag-edit])
        ;; folders
        (if (-> folder-show :folders)
          [:main {:class "pt-3 px-4"}
           [pager (-> folder-show :nav)]
           [cards (-> folder-show :folders)]
           [pager (-> folder-show :nav)]]
          [loading])]])}))
