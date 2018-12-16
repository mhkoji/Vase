(ns vase.gui.browser.pages.folder.render
  (:require [goog.events :as gevents]
            [goog.dom :as gdom]
            [goog.Uri :as guri]
            [goog.History]
            [vase.gui.browser.pages.folder.drivers.list]
            [vase.gui.browser.pages.folder.drivers.viewer]
            [vase.gui.browser.util]))

(defn get-uri []
  (goog.Uri.
   (let [hash (.-hash js/location)]
     (if (= hash "") "" (subs hash 1)))))

(defn show-internal [elem folder-id]
  (vase.gui.browser.util/render-loop
   (let [uri (get-uri)]
     (cond (= (.getPath uri) "spread")
           (vase.gui.browser.pages.folder.drivers.viewer/create-double
            elem
            folder-id)
           (= (.getPath uri) "single")
           (vase.gui.browser.pages.folder.drivers.viewer/create-single
            elem
            folder-id)
           :else
           (vase.gui.browser.pages.folder.drivers.list/create
            elem
            folder-id)))))

(defn render-loop [elem {:keys [folder-id]}]
  (let [history (goog.History.)]
    (gevents/listen history
                    goog.History/EventType.NAVIGATE
                    #(show-internal elem folder-id))
    (.setEnabled history true))
  (show-internal elem folder-id))
