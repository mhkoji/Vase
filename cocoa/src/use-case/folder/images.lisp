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
(defun list-images (folder-id &key from size folder-repository)
  ;@type! folder-id !integer
  ;@type! from integer 0
  ;@type! size integer 100
  ;@type! folder-repository !folder-repository
  (ensure-integer! from 0)
  (ensure-integer! size 100)
  (let* ((folder (car (list-folders/ids folder-repository
                                        (make-list-spec)
                                        (list folder-id))))
         (query (folder-content-query folder :from from :size size))
         (contents (cocoa.entity.folder:list-contents-by-query
                    folder-repository
                    query)))
    (mapcar #'content->image-dto contents)))