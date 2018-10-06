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

(defn page [{:keys [store]}]
  [:div
   ;; header
   [header (read-folders/store-header-state store)]
   [:main {:class "pt-3 px-4"}
    [:h1 {:class "h2"} "Folders"]
    ;; modal (in document)
    (when-let [tag (read-folders/store-tag-state store)]
      [modal-editing-tag tag])
    ;; folders
    (if-let [body (read-folders/store-body-state store)]
      [:main {:class "pt-3 px-4"}
       [pager (-> body :nav)]
       [cards (-> body :folders)]
       [pager (-> body :nav)]]
      [loading])]])


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
    (r/render [page {:store store}] elem)))

(defn show [elem _]
  (render-iter {:create create-store :render (create-renderer elem)}))
