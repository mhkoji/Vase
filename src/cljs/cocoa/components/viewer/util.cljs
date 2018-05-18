(ns cocoa.components.viewer.util)

(defn set-max-size! [elem & {:keys [width height]}]
  (let [style (.-style elem)]
    (when width
      (set! (.-maxWidth style) (str width "px")))
    (when height
      (set! (.-maxHeight style) (str height "px")))))

(defn forward-keydown-p [evt]
  (let [key-code (.-keyCode evt)]
    (or (= key-code 32) (= key-code 39) (= key-code 40))))

(defn forward-wheel-p [evt]
  (< 0 (.-deltaY evt)))
