(ns vase.entities.folder)

(defrecord Image [image-id url])

(defrecord Thumbnail [image-id url])

(defrecord Folder [folder-id name thumbnail])
