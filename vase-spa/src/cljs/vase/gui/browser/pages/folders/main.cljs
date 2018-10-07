(ns vase.gui.browser.pages.folders.main
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as gdom]
            [bidi.bidi :refer [match-route]]
            [reagent.core :as r]
            [vase.gui.browser.pages.folders.store :as store]
            [vase.gui.browser.pages.folders.view :as view]
            [vase.gui.browser.util :refer [render-iter]]))

(defn create-renderer [elem]
  (fn [store]
    (r/render [view/page {:header-state
                          (store/store-header-state store)
                          :body-state
                          (store/store-body-state store)
                          :tag-state
                          (store/store-tag-state store)}]
              elem)))

(defn show [elem _]
  (render-iter {:create store/create
                :render (create-renderer elem)}))

(enable-console-print!)

(def *routes*
  ["/" {"folders" show}])

(let [{:keys [handler route-params]}
      (match-route *routes* (.-pathname js/location))]
  (handler (gdom/getElement "app") route-params))
