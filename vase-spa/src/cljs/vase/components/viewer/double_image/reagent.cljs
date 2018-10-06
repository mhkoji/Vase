(ns vase.components.viewer.double-image.reagent
  (:require  [goog.events :as gevents]
             [reagent.core :as r]
             [vase.components.viewer.util
              :refer [set-max-size! forward-keydown-p forward-wheel-p]]
             [vase.components.progress.reagent :as reagent-progress]))

(defn resize-viewer! [left-elem right-elem thumbnail-elem
                      {:keys [height width]}]
  (when (and left-elem right-elem thumbnail-elem)
    (let [image-width (/ (- width (.-offsetWidth thumbnail-elem)) 2)]
      (set-max-size! left-elem  :width image-width :height height)
      (set-max-size! right-elem :width image-width :height height))
    (set-max-size! thumbnail-elem :height height)))

(defn make-diff [forward-p]
  (if forward-p 2 -2))

(defn double-image-viewer [{:keys [on-diff]}]
  (let [left-elem      (atom nil)
        right-elem     (atom nil)
        thumbnail-elem (atom nil)
        on-keydown     (comp on-diff make-diff forward-keydown-p)
        on-wheel       (comp on-diff make-diff forward-wheel-p)]
    (r/create-class
     {:component-did-mount
      (fn [comp]
        (resize-viewer! @left-elem @right-elem @thumbnail-elem
                        (-> (r/props comp) :size))
        (.addEventListener
         js/window gevents/EventType.KEYDOWN on-keydown))

      :component-did-update
      (fn [comp prev-props prev-state]
        (resize-viewer! @left-elem @right-elem @thumbnail-elem
                        (-> (r/props comp) :size)))

      :component-will-unmount
      (fn [comp]
        (.removeEventListener
         js/window gevents/EventType.KEYDOWN on-keydown))

      :reagent-render
      (fn [{:keys [spread-urls thumbnails viewer-select-list progress
                   on-diff]}]
        [:div {:class "vase-component-doubleimageviewer"}

         [:div {:class "vase-component-doubleimageviewer-left"
                :style {:position "absolute" :float "left"}}
          (let [{:keys [left right]} spread-urls]
            [:div
             [:div {:on-wheel on-wheel :style {:display "inline-block"}}
              [:img {:src left :ref #(reset! left-elem %)}]]
             [:div {:onWheel on-wheel :style {:display "inline-block"}}
              [:img {:src right :ref #(reset! right-elem %)}]]])]

         [:div {:class "vase-component-doubleimageviewer-right"}
          [:div {:ref #(reset! thumbnail-elem %)
                 :class "vase-component-standardviewer-thumbnails"}

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

           (let [base-name "vase-component-standardviewer-thumbnail"]
             (for [{:keys [id src link-to highlighted-p]} thumbnails]
               ^{:key id}
               [:div {:class (str base-name
                                  (if highlighted-p
                                    (str " " base-name "-highlighted")
                                    ""))}
                [:a {:href link-to}
                 [:img {:src src :class base-name}]]]))

           (reagent-progress/progress progress)]]])})))
