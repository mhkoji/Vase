(ns cocoa.controller.nav.reagent)

(defn link [{:keys [link enabled]} & children]
  [:a {:href link
       :class (str "btn" (if enabled "" " disabled"))}
   children])

(defn icon-prev []
  [:span {:key "left" :class "oi oi-chevron-left"}])

(defn icon-next []
  [:span {:key "right" :class "oi oi-chevron-right"}])

(defn pager [{:keys [prev next]}]
  [:div {:class "container"}
   [:div {:class "btn-toolbar" :role "toolbar"}
    [link prev ^{:key "prev"} [icon-prev]]
    [link next ^{:key "next"} [icon-next]]]])
