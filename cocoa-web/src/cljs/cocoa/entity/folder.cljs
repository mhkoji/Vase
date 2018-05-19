(ns cocoa.entity.folder)

(defn set-name [folder name]
  (assoc folder :name name))

(defn set-images [folder images]
  (assoc folder :images images))


(defrecord repository [folders])

(defn make-repository []
  (repository. []))

(defn clear [_]
  (repository. nil))

(defn find-folder-by-id [repository folder-id]
  (let [folders (:folders repository)]
    (first (filter #(= (:folder-id %) folder-id) folders))))

(defn find-all [repository]
  (-> repository :folders))

(defn add-all [repository folders]
  (let [new-folders
        (filter #(not (find-folder-by-id repository
                                         (-> % :folder-id)))
                folders)]
    (update repository :folders concat new-folders)))

(defn save-folder [repository folder]
  (if-let [index (loop [i 0 folders (:folders repository)]
                   (cond (empty? folders)
                         nil
                         (= (:folder-id (first folders))
                            (:folder-id folder))
                         i
                         :else
                         (recur (inc i) (rest folders))))]
    (assoc-in repository [:folders index] folder)
    (update-in repository [:folders] conj folder)))
