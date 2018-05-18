(ns cocoa.presenter.browser.core
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as gdom]
            [bidi.bidi :refer [match-route]]
            [cocoa.presenter.browser.pages.folder]
            [cocoa.presenter.browser.pages.folders]
            [cocoa.presenter.browser.pages.tag-folders]))

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(enable-console-print!)

(def *routes*
  ["/"
   {["folder/" :folder-id]
    cocoa.presenter.browser.pages.folder/show

    "folders"
    cocoa.presenter.browser.pages.folders/show

    ["tag/" :tag-id "/folders"]
    cocoa.presenter.browser.pages.tag-folders/show

    "tags"
    cocoa.presenter.browser.pages.tag-folders/show}])

(let [{:keys [handler route-params]}
      (match-route *routes* (.-pathname js/location))]
  (handler (gdom/getElement "app") route-params))
