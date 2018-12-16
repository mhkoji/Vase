(ns vase.gui.browser.util
  (:require [cljs.core.async :refer [go <! chan put!]]))


(defn render-loop [{:keys [create-store render]}]
  (let [reducer-chan (chan)]
    (go (loop [store (create-store #(put! reducer-chan %))]
          (render store)
          (let [update-store (<! reducer-chan)]
            (recur (update-store store)))))))
