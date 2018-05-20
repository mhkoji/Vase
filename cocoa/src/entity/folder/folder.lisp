(defpackage :cocoa.entity.folder
  (:use :cl))
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

;;; The thumbnail of a folder
;; Get the unique id of the thumbnail
@export
(defgeneric thumbnail-id (thumbnail))


;;; The representation of each content in a folder
;; Get the unique id of a content
@export
(defgeneric content-id (content))


;;; Folder
;; Get the unique id of a content
@export
(defgeneric folder-id (folder))

;; Get the name of a folder
@export
(defgeneric folder-name (folder))

;; Get the thumbnail of a folder
@export
(defgeneric folder-thumbnail (folder))

;; Get the contents in a folder
@export
(defgeneric folder-contents (folder))


;; What a folder is made from
(defstruct source folder-id name thumbnail contents modified-at)
(export 'make-source)
(export 'source-folder-id)
(export 'source-name)
(export 'source-path)
(export 'source-thumbnail)
(export 'source-contents)
(export 'source-modified-at)

;; Save folders with sources
@export
(defgeneric save-folders/sources (folder-repository sources))


;; The specification of listing folders
(defstruct list-spec with-thumbnail-p)
(export 'list-spec)
(export 'make-list-spec)
(export 'list-spec-with-thumbnail-p)

;; List folders with the ids by the arguments
@export
(defgeneric list-folders/ids (folder-repository list-spec folder-id-list))

;; List folders in the range by the arguments
@export
(defgeneric list-folders/range (folder-repository list-spec offset size))

;; Delete folders
@export
(defgeneric delete-folders/ids (folder-repository folder-id-list))

