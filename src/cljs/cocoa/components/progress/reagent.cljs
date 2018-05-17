(ns cocoa.components.progress.reagent)

(defn progress [{:keys [now max]}]
  (let [width (* 100 (/ (+ 1 now) max))]
    [:div {:class "progress"}
     [:div {:class (str "progress-bar "
                        "progress-bar-info "
                        "progress-bar-striped")
            :role "progressbar"
            :aria-valuenow now
            :aria-valuemin "0"
            :aria-valuemax max
            :style {:width (str width "%")}}]]))
