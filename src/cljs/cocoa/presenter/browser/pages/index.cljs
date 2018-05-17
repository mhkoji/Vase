(ns cocoa.presenter.browser.pages.index
  (:require [cljs.core.async :refer [go <! chan put!]]

            [goog.Uri :as guri]
            [goog.History]
            [goog.events :as gevents]
            [cljs.reader :refer [read-string]]

            [cocoa.controller.read-folder.by-list
             :as read-folder-by-list]
            [cocoa.controller.read-folder.by-spread
             :as read-folder-by-spread]
            [cocoa.controller.read-folder.by-single-image-viewer
             :as read-folder-by-single-image-viewer]
            [cocoa.controller.read-folders
             :as read-folders]
            [cocoa.controller.tagged-folder-list
             :as tagged-folder-list]

            [reagent.core :as r]
            [cocoa.presenter.browser.pages.folder]
            [cocoa.presenter.browser.pages.folders]
            [cocoa.presenter.browser.pages.tag-folders]))

(defn render-iter [{:keys [create render]}]
  (let [reducer-chan (chan)]
    (go (loop [store (create #(put! reducer-chan %))]
          (render store)
          (let [update-store (<! reducer-chan)]
            (recur (update-store store)))))))

(defn folders-render [params elem]
  (render-iter
   {:create (letfn [(search->range [search]
                      (let [query-data (guri/QueryData.
                                        (if (= search "")
                                          ""
                                          (subs search 1)))]
                        (list (read-string (.get query-data "from" "0"))
                              (read-string (.get query-data "size" "500")))))]
              (let [[from size] (search->range (.-search js/location))]
                #(read-folders/create-store % from size)))
    :render #(r/render
              [cocoa.presenter.browser.pages.folders/page
               {:header (read-folders/store-header-state %)
                :body (read-folders/store-body-state %)
                :tag (read-folders/store-tag-state %)
                :load-folders (fn [] (read-folders/store-load-folders %))}]
              elem)}))


(defn folder-render-internal [folder-id elem]
  (render-iter
   (let [uri (goog.Uri. (let [hash (.-hash js/location)]
                          (if (= hash "") "" (subs hash 1))))]
     (cond (= (.getPath uri) "spread")
           {:create #(read-folder-by-spread/create-store % folder-id nil)
            :render #(r/render
                      [cocoa.presenter.browser.pages.folder/spread-page
                       (read-folder-by-spread/store-state %)]
                      elem)}
           (= (.getPath uri) "single")
           {:create #(read-folder-by-single-image-viewer/create-store % folder-id nil)
            :render #(r/render
                      [cocoa.presenter.browser.pages.folder/single-image-viewer-page
                       (read-folder-by-single-image-viewer/store-state %)]
                      elem)}
           :else
           {:create #(read-folder-by-list/create-store % folder-id nil)
            :render #(r/render
                      [cocoa.presenter.browser.pages.folder/list-page
                       (read-folder-by-list/store-state %)]
                      elem)}))))

(defn folder-render [{:keys [folder-id]} elem]
  (let [history (goog.History.)]
    (gevents/listen history
                    goog.History/EventType.NAVIGATE
                    #(folder-render-internal folder-id elem))
    (.setEnabled history true))
  (folder-render-internal folder-id elem))


(defn tag-folders-render [{:keys [tag-id]} elem]
  (render-iter
   {:create #(tagged-folder-list/create-store % tag-id)
    :render #(r/render
              [cocoa.presenter.browser.pages.tag-folders/page
               (tagged-folder-list/store-state %)]
              elem)}))
