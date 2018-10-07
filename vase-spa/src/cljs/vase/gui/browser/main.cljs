(ns vase.gui.browser.main
  (:require [bidi.bidi :refer [match-route]]
            [goog.dom :as gdom]
            [vase.gui.browser.pages.folder.render]
            [vase.gui.browser.pages.folders]
            [vase.gui.browser.pages.tags.folders]))

(def *routes*
  ["/"
   {["folder/" :folder-id]
    vase.gui.browser.pages.folder.render/render-loop

    "folders"
    vase.gui.browser.pages.folders/render-loop

    ["tag/" :tag-id "/folders"]
    vase.gui.browser.pages.tags.folders/render-loop

    "tags"
    vase.gui.browser.pages.tags.folders/render-loop}])

(let [{:keys [handler route-params]}
      (match-route *routes* (.-pathname js/location))]
  (handler (gdom/getElement "app") route-params))
