(ns vase.gui.browser.pages.folders
  (:require [clojure.browser.repl :as repl]
            [goog.dom :as gdom]
            [reagent.core :as r]
            [vase.entities.folder]
            [vase.entities.transaction :refer [state]]
            [vase.use-cases.show-folders]
            [vase.use-cases.edit-folder-tags]
            [vase.gui.controllers.edit-folder-tags]
            [vase.gui.components.nav.state :as nav-state]
            [vase.gui.components.folder.state :as folder-state]
            [vase.gui.components.pages.folders]
            [vase.gui.browser.util]
            [vase.gui.browser.url :as url]))

(defn create-store [update-store]
  {:show-folders
   (vase.entities.transaction/DelegateTransaction.
    (vase.use-cases.show-folders/State. nil nil nil)
    #(update-store
      (fn [s] (update-in s [:show-folders :state] %))))
   :edit-folder-tags
   (vase.entities.transaction/DelegateTransaction.
    (vase.use-cases.edit-folder-tags/State. nil nil nil)
    #(update-store
      (fn [s] (update-in s [:edit-folder-tags :state] %))))})

(defn store-nav-state [store]
  (letfn [(region->nav [{:keys [from size]}]
            (nav-state/make-nav
             (when from (url/folders from size))))]
    (let [state (-> store :show-folders state)]
      (nav-state/state (-> state :prev-region region->nav)
                       (-> state :next-region region->nav)))))

(defn store-folders-state [store]
  (when-let [folders (-> store :show-folders state :folders)]
    (for [f folders]
      (folder-state/state
       (-> f :folder-id)
       (-> f :name)
       (-> f :thumbnail :url)
       #(vase.use-cases.edit-folder-tags/start
         (-> store :edit-folder-tags) (-> f :folder-id))))))

(defn create-renderer [elem]
  (fn [store]
    (r/render [vase.gui.components.pages.folders/page
               {:folder-show
                {:nav
                 (store-nav-state store)
                 :folders
                 (store-folders-state store)
                 :show-folders
                 (fn [from size]
                   (vase.use-cases.show-folders/show
                    (-> store :show-folders)
                    (vase.use-cases.show-folders/make-region from size)))}

                :tag-edit
                (vase.gui.controllers.edit-folder-tags/context-state
                 (-> store :edit-folder-tags))}]
              elem)))


(defn render-loop [elem _]
  (vase.gui.browser.util/render-loop
   {:create-store create-store
    :render (create-renderer elem)}))
