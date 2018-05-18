(ns cocoa.presenter.browser.util
  (:require [cljs.core.async :refer [go <! chan put!]]))

(defn render-iter [{:keys [create render]}]
  (let [reducer-chan (chan)]
    (go (loop [store (create #(put! reducer-chan %))]
          (render store)
          (let [update-store (<! reducer-chan)]
            (recur (update-store store)))))))
