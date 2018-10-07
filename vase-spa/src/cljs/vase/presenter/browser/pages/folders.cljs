(ns vase.presenter.browser.pages.folders
  (:require [goog.Uri :as guri]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [vase.controller.read-folders :as read-folders]
            [vase.components.header.reagent :refer [header]]
            [vase.components.nav.reagent :refer [pager]]
            [vase.components.folder.reagent :refer [cards]]
            [vase.components.tag_editing.reagent :refer [modal-editing-tag]]
            [vase.presenter.browser.util :refer [render-iter]]))

(defn loading []
  [:div "Loading..."])

(defn page [{:keys [header-state tag-state body-state]}]
  [:div
   ;; header
   [header header-state)]
   [:main {:class "pt-3 px-4"}
    [:h1 {:class "h2"} "Folders"]
    ;; modal (in document)
    (when tag-state
      [modal-editing-tag tag-state])
    ;; folders
    (if body-state
      [:main {:class "pt-3 px-4"}
       [pager (-> body :nav)]
       [cards (-> body :folders)]
       [pager (-> body :nav)]]
      [loading])])


(defn create-store [update-store!]
  (let [search (.-search js/location)
        query-data (guri/QueryData. (if (= search "") "" (subs search 1)))
        from (read-string (.get query-data "from" "0"))
        size (read-string (.get query-data "size" "500"))
        store (read-folders/create-store update-store! from size)]
    (read-folders/store-load-folders! store)
    store))

(defn create-renderer [elem]
  (fn [store]
    (r/render [page {:header-state
                     (read-folders/store-header-state stores)
                     :body-state
                     (read-folders/store-body-state store)
                     :tag-state
                     (read-folders/store-tag-state store)}]
              elem)))

(defn show [elem _]
  (render-iter {:create create-store :render (create-renderer elem)}))
