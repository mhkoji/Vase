(ns vase.presenter.browser.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as gdom]
            [bidi.bidi :refer [match-route]]
            [vase.presenter.browser.pages.folder]
            [vase.presenter.browser.pages.folders]
            [vase.presenter.browser.pages.tag-folders]))

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(def *routes*
  ["/"
   {["folder/" :folder-id]
    vase.presenter.browser.pages.folder/show

    "folders"
    vase.presenter.browser.pages.folders/show

    ["tag/" :tag-id "/folders"]
    vase.presenter.browser.pages.tag-folders/show

    "tags"
    vase.presenter.browser.pages.tag-folders/show}])

(let [{:keys [handler route-params]}
      (match-route *routes* (.-pathname js/location))]
  (handler (gdom/getElement "app") route-params))
