(defpackage :cocoa.folder.content
  (:use :cl)
  (:import-from :cl-arrows :->>))
(in-package :cocoa.folder.content)
(cl-annot:enable-annot-syntax)

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

(defun content->resp (content)
  (list :id (cocoa.entity.folder:content->image-id content)))

@export
(defun get-images (folder-id &key from size
                                  folder-repository
                                  folder-content-repository)
  "The use case of listing images in a folder"
  ;@type! folder-repos !folder-repository
  ;@type! folder-id !integer
  ;@type! from integer 0
  ;@type! size integer 100
  (ensure-integer! from 0)
  (ensure-integer! size 100)
  (cocoa.folder.util:accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder:load-folders-by-ids
                      folder-repository
                      (list folder-id)))))
    (->> (cocoa.entity.folder:folder-contents folder-content-repository
          folder :from from :size size)
         (remove-if-not
          #'cocoa.entity.folder:content->image-id)
         (mapcar #'content->resp))))

@export
(defun append-contents (folder-id contents
                        &key folder-repository
                             folder-content-repository)
  (cocoa.folder.util:accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder:load-folders-by-ids
                      folder-repository
                      (list folder-id)))))
    (let ((appending (cocoa.entity.folder:make-appending
                      :folder folder
                      :contents contents)))
      (cocoa.entity.folder:update-folder-contents
       folder-content-repository appending))))
