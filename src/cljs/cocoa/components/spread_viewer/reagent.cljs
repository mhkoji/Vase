(ns cocoa.components.spread-viewer.reagent
  (:require  [goog.events :as gevents]
             [reagent.core :as r]))

(defn set-max-size! [elem & {:keys [width height]}]
  (let [style (.-style elem)]
    (when width
      (set! (.-maxWidth style) (str width "px")))
    (when height
      (set! (.-maxHeight style) (str height "px")))))

(defn resize-viewer! [left-elem right-elem thumbnail-elem
                      {:keys [height width]}]
  (when (and left-elem right-elem thumbnail-elem)
    (let [image-width (/ (- width (.-offsetWidth thumbnail-elem)) 2)]
      (set-max-size! left-elem  :width image-width :height height)
      (set-max-size! right-elem :width image-width :height height))
    (set-max-size! thumbnail-elem :height height)))

(defn make-diff [forward-p]
  (if forward-p 2 -2))

(defn forward-keydown-p [evt]
  (let [key-code (.-keyCode evt)]
    (or (= key-code 32) (= key-code 39) (= key-code 40))))

(defn spread-viewer [{:keys [on-diff]}]
  (let [left-elem      (atom nil)
        right-elem     (atom nil)
        thumbnail-elem (atom nil)
        on-keydown     #(on-diff (make-diff (forward-keydown-p %)))]
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
      (fn [{:keys [spread-urls thumbnails progress on-diff]}]
        [:div {:class "cocoa-component-doubleimageviewer"}

         [:div {:class "cocoa-component-doubleimageviewer-left"
                :style {:position "absolute" :float "left"}}
          (let [{:keys [left right]} spread-urls
                on-wheel #(on-diff (make-diff (< 0 (.-deltaY %))))]
            [:div
             [:div {:on-wheel on-wheel :style {:display "inline-block"}}
              [:img {:src left :ref #(reset! left-elem %)}]]
             [:div {:onWheel on-wheel :style {:display "inline-block"}}
              [:img {:src right :ref #(reset! right-elem %)}]]])]

         [:div {:class "cocoa-component-doubleimageviewer-right"}
          [:div {:ref #(reset! thumbnail-elem %)
                 :class "cocoa-component-standardviewer-thumbnails"}

           (let [base-name "cocoa-component-standardviewer-thumbnail"]
             (for [{:keys [id src link-to highlighted-p]} thumbnails]
               ^{:key id}
               [:div {:class (str base-name
                                  (if highlighted-p
                                    (str " " base-name "-highlighted")
                                    ""))}
                [:a {:href link-to}
                 [:img {:src src :class base-name}]]]))

           (let [{:keys [now max]} progress
                 width (* 100 (/ (+ 1 now) max))]
             [:div {:class "progress"}
              [:div {:class (str "progress-bar "
                                 "progress-bar-info "
                                 "progress-bar-striped")
                     :role "progressbar"
                     :aria-valuenow now
                     :aria-valuemin "0"
                     :aria-valuemax max
                     :style {:width (str width "%")}}]])]]])})))
