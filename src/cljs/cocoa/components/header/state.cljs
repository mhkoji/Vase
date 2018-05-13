(ns cocoa.components.header.state
  (:require [cocoa.presenter.browser.url :as url]))

(defn get-state [in-page]
  {:brand {:name "Cocoa" :url (url/folders)}
   :pages [{:id "folders"
            :name "Folders"
            :url (url/folders)
            :active-p (= in-page :folder)}
           {:id "tag"
            :name "Tags"
            :url (url/tags)
            :active-p (= in-page :tag)}]})
