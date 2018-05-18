(ns cocoa.components.viewer.single-image.reagent
  (:require  [goog.events :as gevents]
             [reagent.core :as r]
             [cocoa.components.viewer.util
              :refer [set-max-size! forward-keydown-p forward-wheel-p]]
             [cocoa.components.progress.reagent :as reagent-progress]))

(defn resize-viewer! [image-elem thumbnail-elem {:keys [height width]}]
  (when (and image-elem thumbnail-elem)
    (set-max-size! image-elem
                   :width (- width (.-offsetWidth thumbnail-elem))
                   :height height)
    (set-max-size! thumbnail-elem
                   :height height)))

(defn make-diff [forward-p]
  (if forward-p 1 -1))

(defn single-image-viewer [{:keys [on-diff]}]
  (let [image-elem     (atom nil)
        thumbnail-elem (atom nil)
        on-keydown     (comp on-diff make-diff forward-keydown-p)
        on-wheel       (comp on-diff make-diff forward-wheel-p)]
    (r/create-class
     {:component-did-mount
      (fn [comp]
        (resize-viewer! @image-elem @thumbnail-elem
                        (-> (r/props comp) :size))
        (.addEventListener
         js/window gevents/EventType.KEYDOWN on-keydown))

      :component-did-update
      (fn [comp prev-props prev-state]
        (resize-viewer! @image-elem @thumbnail-elem
                        (-> (r/props comp) :size)))

      :component-will-unmount
      (fn [comp]
        (.removeEventListener
         js/window gevents/EventType.KEYDOWN on-keydown))

      :reagent-render
      (fn [{:keys [image-url thumbnails progress viewer-select-list
                   on-diff]}]
        [:div {:class "cocoa-component-singleimageviewer"}

         [:div {:class "cocoa-component-singleimageviewer-left"
                :style {:position "absolute" :float "left"}}
          [:div {:class "cocoa-component-singleimageviewer-image"}
           [:div {:on-wheel on-wheel :style {:display "inline-block"}}
            [:img {:src image-url :ref #(reset! image-elem %)}]]]]

         [:div {:class "cocoa-component-singleimageviewer-right"}
          [:div {:ref #(reset! thumbnail-elem %)
                 :class "cocoa-component-standardviewer-thumbnails"}

           [:div {:class "dropdown"}
            [:a {:class "btn btn-secondary dropdown-toggle"
                 :href "#"
                 :role "button"
                 :id "dropdownMenuLink"
                 :data-toggle "dropdown"
                 :aria-haspopup "true"
                 :aria-expanded "false"}
             (-> viewer-select-list first :name)]

            [:div {:class "dropdown-menu"
                   :aria-labelledby "dropdownMenuLink"}
             (for [{:keys [id name link]} (rest viewer-select-list)]
               ^{:key id}
               [:a {:class "dropdown-item" :href link}
                name])]]

           (let [base-name "cocoa-component-standardviewer-thumbnail"]
             (for [{:keys [id src link-to highlighted-p]} thumbnails]
               ^{:key id}
               [:div {:class (str base-name
                                  (if highlighted-p
                                    (str " " base-name "-highlighted")
                                    ""))}
                [:a {:href link-to}
                 [:img {:src src :class base-name}]]]))

           (reagent-progress/progress progress)]]])})))
