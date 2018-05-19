(ns cocoa.controller.read-folders
  (:require [cljs.core.async :refer [go <!]]

            [cocoa.infra.api.folder :as folder-api]
            [cocoa.entity.folder :as folder]
            [cocoa.use-case.load-folders :as load-folders-use-case]

            [cocoa.controller.edit-folder-tags :as edit-folder-tags]
            [cocoa.components.header.state :as header-state]
            [cocoa.components.nav.state :as nav-state]
            [cocoa.components.folder.state :as folder-state]

            [cocoa.presenter.browser.url :as url]))

(defn create-store [update-store! from size]
  {:const
   (letfn [(make-range [from size]
             {:from from :size size})]
     {:ranges {:prev (make-range (- from size) size)
               :curr (make-range from size)
               :next (make-range (+ from size) size)}})
   :substore
   {:edit-folder-tags
    (edit-folder-tags/create-store
     #(update-store!
       (fn [s] (update-in s [:substore :edit-folder-tags] %))))}
   :db
   {:folder (folder/make-repository)}
   :update-db
   {:folder
    #(update-store! (fn [s] (update-in s [:db :folder] %)))}})

(defn store-nav-state [store]
  (letfn [(range->nav [{:keys [from size]}]
            (nav-state/make-nav (when (<= 0 from)
                                  (url/folders from size))))]
    (nav-state/state (-> store :const :ranges :prev range->nav)
                     (-> store :const :ranges :next range->nav))))

(defn store-folders-state [store]
  (when-let [folders (-> store :db :folder folder/find-all)]
    (for [f folders]
      (folder-state/state (-> f :folder-id)
                          (-> f :name)
                          (-> f :thumbnail :url)
                          #(edit-folder-tags/store-start-edit
                            (-> store :substore :edit-folder-tags)
                            (-> f :folder-id))))))

(defn store-body-state [store]
  (when-let [folders-state (store-folders-state store)]
    {:nav (store-nav-state store) :folders folders-state}))

(defn store-header-state [store]
  (header-state/get-state :folder))

(defn store-tag-state [store]
  (edit-folder-tags/store-state (-> store :substore :edit-folder-tags)))

(defn store-load-folders! [store]
  (load-folders-use-case/load-all (-> store :update-db :folder)
                                  (-> store :const :ranges :curr)))
