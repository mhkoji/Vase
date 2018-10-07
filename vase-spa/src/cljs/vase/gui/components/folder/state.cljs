(ns vase.gui.components.folder.state)

(defn state [folder-id name thumbnail-url on-edit-tag]
  {:folder-id     folder-id
   :name          name
   :thumbnail-url thumbnail-url
   :on-edit-tag   on-edit-tag})
