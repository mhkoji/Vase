(ns cocoa.presenter.browser.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as gdom]
            [bidi.bidi :refer [match-route]]
            [cocoa.presenter.browser.pages.index :as index]))

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(def *routes*
  ["/"
   {["folder/" :folder-id]
    (fn [params elem]
      (index/folder-render params elem))

    "folders"
    (fn [params elem]
      (index/folders-render params elem))

    ["tag/" :tag-id "/folders"]
    (fn [params elem]
      (index/tag-folders-render params elem))

    "tags"
    (fn [params elem]
      (index/tag-folders-render {} elem))}])

(let [{:keys [handler route-params]}
      (match-route *routes* (.-pathname js/location))]
  (handler route-params (gdom/getElement "app")))
