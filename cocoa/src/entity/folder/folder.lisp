(defpackage :cocoa.entity.folder
  (:use :cl))
(in-package :cocoa.entity.folder)
(cl-annot:enable-annot-syntax)

;;; The thumbnail of a folder
;; Get the unique id of the thumbnail
@export
(defgeneric thumbnail-id (thumbnail))

;; A simple implementaion of thumbnail
(defclass simple-thumbnail ()
  ((thumbnail-id
    :initarg :thumbnail-id
    :reader thumbnail-id)))

@export
(defun make-simple-thumbnail (id)
  (make-instance 'simple-thumbnail :thumbnail-id id))


;;; Content in a folder
;; Get the unique id of a content
@export
(defgeneric content-id (content))

;; A simple simplementation of content
(defclass simple-content ()
  ((content-id
    :initarg :content-id
    :reader content-id)))

@export
(defun make-simple-content (id)
  (make-instance 'simple-content :content-id id))


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

;; List folders with the ids by the arguments
@export
(defgeneric list-folders/ids (folder-repository folder-id-list))

;; List folders in the range by the arguments
@export
(defgeneric list-folders/range (folder-repository offset size))

;; Delete folders
@export
(defgeneric delete-folders/ids (folder-repository folder-id-list))


@export
(defgeneric query-folder-thumbnails (folder-repository folders))
@export
(defgeneric get-folder-thumbnail (thumbnail-result folder))


@export
(defstruct list-spec with-thumbnail-p)

@export
(defgeneric list-folders/spec-range (dao list-spec ids))
