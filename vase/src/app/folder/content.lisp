(defpackage :vase.app.folder.content
  (:use :cl))
(in-package :vase.app.folder.content)

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

(defun content->resp (content)
  (list :id (vase.entities.folder.content:content->image-id content)))

(defun get-images (db folder-id &key from size)
  "The use case of listing images in a folder"
  ;@type! db !db
  ;@type! folder-id !integer
  ;@type! from integer 0
  ;@type! size integer 100
  (ensure-integer! from 0)
  (ensure-integer! size 500)
  (vase.app.folder.util:accept-folder-id folder-id)
  (let ((folder (car (vase.entities.folder.repository:load-by-ids
                      db
                      (list folder-id)))))
    (let ((contents
           (vase.entities.folder.content.repository:folder-contents
            db folder :from from :size size)))
      (let ((image-contents
             (remove-if-not #'vase.entities.folder.content:content->image-id
                            contents)))
        (mapcar #'content->resp image-contents)))))
(export 'get-images)

#+nil
(defun append-contents (folder-id contents &key db)
  (vase.app.folder.util:accept-folder-id folder-id)
  (let ((folder
         (car (vase.entities.folder.repository:load-by-ids db
               (list folder-id)))))
    (let ((appending
           (vase.entities.folder.content.op:make-appending
            :folder folder
            :contents contents)))
      (vase.entities.folder.content.repository:update db appending))))
