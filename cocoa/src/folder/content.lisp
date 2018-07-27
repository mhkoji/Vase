(defpackage :cocoa.folder.content
  (:use :cl))
(in-package :cocoa.folder.content)

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

(defun content->resp (content)
  (list :id (cocoa.entity.folder.content:content->image-id content)))

(defun get-images (folder-id &key db from size)
  "The use case of listing images in a folder"
  ;@type! db !db
  ;@type! folder-id !integer
  ;@type! from integer 0
  ;@type! size integer 100
  (ensure-integer! from 0)
  (ensure-integer! size 100)
  (cocoa.folder.util:accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder.repository:load-by-ids
                      db
                      (list folder-id)))))
    (let ((contents
           (cocoa.entity.folder.content.repository:folder-contents
            db folder :from from :size size)))
      (let ((image-contents
             (remove-if-not #'cocoa.entity.folder.content:content->image-id
                            contents)))
        (mapcar #'content->resp image-contents)))))
(export 'get-images)

#+nil
(defun append-contents (folder-id contents &key db)
  (cocoa.folder.util:accept-folder-id folder-id)
  (let ((folder
         (car (cocoa.entity.folder.repository:load-by-ids db
               (list folder-id)))))
    (let ((appending
           (cocoa.entity.folder.content.op:make-appending
            :folder folder
            :contents contents)))
      (cocoa.entity.folder.content.repository:update db appending))))
