(ns cocoa.presenter.browser.url)

(defn folder [folder-id]
  (str "/folder/" folder-id))

(defn folders
  ([]
   "/folders")
  ([from size]
   (str "/folders?from=" (max 0 from) "&size=" size)))

(defn read-folder-by-spread [folder-id image-id]
  (str "/folder/" folder-id "#spread?image=" image-id))

(defn read-folder-by-single [folder-id image-id]
  (str "/folder/" folder-id "#single?image=" image-id))

(defn tags []
  "/tags")

(defn tag-folders [tag-id]
  (str "/tag/" tag-id "/folders"))
