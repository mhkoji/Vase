(ns cocoa.components.viewer.single-image.reagent
  (:require  [goog.events :as gevents]
             [reagent.core :as r]

             [cocoa.components.spread-viewer.reagent
              :refer [forward-keydown-p]]))

(defn set-max-size! [elem & {:keys [width height]}]
  (let [style (.-style elem)]
    (when width
      (set! (.-maxWidth style) (str width "px")))
    (when height
      (set! (.-maxHeight style) (str height "px")))))

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
        on-keydown     #(on-diff (make-diff (forward-keydown-p %)))]
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
      (fn [{:keys [image-url thumbnails progress on-diff]}]
        [:div {:class "cocoa-component-singleimageviewer"}

         [:div {:class "cocoa-component-singleimageviewer-left"
                :style {:position "absolute" :float "left"}}
          [:div {:class "cocoa-component-singleimageviewer-image"}
           [:div {:on-wheel #(on-diff (make-diff (< 0 (.-deltaY %))))
                  :style {:display "inline-block"}}
            [:img {:src image-url :ref #(reset! image-elem %)}]]]]

         [:div {:class "cocoa-component-singleimageviewer-right"}
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
              [:div {:class "progress-bar"
                     :style {:width (str width "%")}
                     :role "progressbar"
                     :aria-valuenow width
                     :aria-valuemin "0"
                     :aria-valuemax "100"}]])]]])})))
