(ns vase.gui.components.progress.reagent)

(defn progress [{:keys [now max]}]
  (let [width (* 100 (/ (+ 1 now) max))]
    [:div {:class "progress"}
     [:div {:class "progress-bar"
            :style {:width (str width "%")}
            :role "progressbar"
            :aria-valuenow width
            :aria-valuemin "0"
            :aria-valuemax "100"}]]))
