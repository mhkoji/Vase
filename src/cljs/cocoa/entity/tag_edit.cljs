(ns cocoa.entity.tag-edit)

(defrecord edit [attached-ids])

(defn attach [edit tag-id]
  (update edit :attached-ids conj tag-id))

(defn detach [edit tag-id]
  (update edit :attached-ids (fn [ids] (remove #(= tag-id %) ids))))

(defn list-attached-ids [edit]
  (:attached-ids edit))


(defrecord repository [edit])

(defn save-edit [repository edit]
  (assoc repository :edit edit))

(defn find-edit [repository]
  (:edit repository))
