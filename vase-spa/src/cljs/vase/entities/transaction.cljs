(ns vase.entities.transaction)

(defprotocol Transaction
  (state [this])
  (update-state [this f]))


(defrecord CallbackTransaction [state update-state-fn]
  Transaction
  (state [this]
    (-> this :state))
  (update-state [this f]
    ((-> this :update-state-fn) f)))
