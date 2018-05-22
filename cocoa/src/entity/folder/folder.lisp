;;; The representation of Folder
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

@export
(defgeneric folder-id (folder)
  (:documentation "Returns the unique id of a content"))

@export
(defgeneric folder-name (folder)
  (:documentation "Returns the name of a folder"))

@export
(defgeneric folder-thumbnail (folder)
  (:documentation "Returns the thumbanil of a folder"))

@export
(defgeneric folder-contents (folder)
  (:documentation "Returns the contents in a folder"))


;; What a folder is made from
(defstruct source folder-id name thumbnail contents modified-at)
(export 'make-source)
(export 'source-folder-id)
(export 'source-name)
(export 'source-path)
(export 'source-thumbnail)
(export 'source-contents)
(export 'source-modified-at)

@export
(defgeneric save-folders/sources (folder-repository sources)
  (:documentation "Save folders"))


;; The specification of listing folders
(defstruct list-spec with-thumbnail-p)
(export 'list-spec)
(export 'make-list-spec)
(export 'list-spec-with-thumbnail-p)

@export
(defgeneric list-folders/ids (folder-repository list-spec folder-id-list)
  (:documentation "Returns the folders with the given ids"))

@export
(defgeneric list-folders/range (folder-repository list-spec offset size)
  (:documentation "Returns the folders within the range"))

@export
(defgeneric delete-folders/ids (folder-repository folder-id-list)
  (:documentation "Delete the folders"))

@export
(defgeneric search-folders/name (folder-repository list-spec keyword)
  (:documentation "Returns the folders whose names contain the keyword"))
