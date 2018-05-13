(ns cocoa.components.nav.state)

(defn make-nav [link]
  (if link
    {:link link :enabled true}
    {:link "" :enabled false}))

(defn state [prev-nav next-nav]
  {:prev prev-nav :next next-nav})
