(ns cocoa.entity.tag)

(defn set-name [tag name]
  (assoc tag :name name))

(defn set-contents [tag contents]
  (assoc tag :contents contents))

(defrecord repository [tags])

(defn add-all [repository new-tags]
  (update repository :tags #(into [] (concat % new-tags))))

(defn save-tag [repository tag]
  (if-let [index (loop [i 0 tags (:tags repository)]
                   (if (= (:tag-id (first tags)) (:tag-id tag))
                     i
                     (recur (inc i) (rest tags))))]
    (assoc-in repository [:tags index] tag)
    repository))


(defn find-all [repository]
  (:tags repository))

(defn find-tag [repository tag-id]
  (first (filter #(= (:tag-id %) tag-id) (:tags repository))))


(defn delete-all [repository]
  (assoc repository :tags ()))

(defn delete-tag [repository tag-id]
  (update repository :tags
          (fn [tags] (remove #(= (:tag-id %) tag-id) tags))))

