(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

(defun content->image-dto (content)
  (list :id (cocoa.use-case.folder.content:content->image-id content)))

;;; folder images
@export
(defun list-images (folder-id &key from size folder-dao)
  ;@type! folder-id !integer
  ;@type! from integer 0
  ;@type! size integer 100
  ;@type! folder-dao !folder-dao
  (ensure-integer! from 0)
  (ensure-integer! size 100)
  (let ((folder (car (cocoa.entity.folder:list-by-ids
                      folder-dao
                      (cocoa.entity.folder:make-list-spec)
                      (list folder-id)))))
    (let ((contents (cocoa.entity.folder:list-contents
                     folder
                     :from from :size size)))
      (mapcar #'content->image-dto contents))))
