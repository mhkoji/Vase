(defpackage :cocoa.use-case.folder
  (:use :cl)
  (:import-from :cl-arrows :-> :->> :-<>))
(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

;;;; Get
(defun content->image-dto (content)
  (list :id (cocoa.use-case.folder.content:content->image-id content)))

(defun thumbnail->dto (thumbnail)
  (list :id (cocoa.use-case.folder.thumbnail:thumbnail->image-id
             thumbnail)))

(defun folder->dto (folder)
  (list :id (cocoa.folder:folder-id folder)
        :name (cocoa.folder:folder-name folder)
        :thumbnail (thumbnail->dto (cocoa.folder:folder-thumbnail folder))))

@export
(defun list-by-range (folder-repos)
  (lambda (&key from size)
    (->> (cocoa.folder:load-folders-by-range folder-repos from size)
         (mapcar #'folder->dto))))

@export
(defun list-by-ids (folder-repos)
  (lambda (ids)
    (->> (cocoa.folder:load-folders-by-ids folder-repos ids)
         (mapcar #'folder->dto))))

@export
(defun search-by-name (folder-repos)
  (lambda (name)
    (->> (cocoa.folder:search-folders-by-name folder-repos name)
         (mapcar #'folder->dto))))


@export
(defun get-by-id (folder-repos)
  (lambda (id)
    (->> (car (cocoa.folder:load-folders-by-ids folder-repos (list id)))
         folder->dto)))


;;;; Get contents

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

@export
(defun list-images (folder-repos)
  ;@type! folder-repos !folder-repository
  (lambda (folder-id &key from size)
    "The use case of listing images in a folder"
    ;@type! folder-id !integer
    ;@type! from integer 0
    ;@type! size integer 100
    (ensure-integer! from 0)
    (ensure-integer! size 100)
    (let ((folder (car (cocoa.folder:load-folders-by-ids
                        folder-repos
                        (list folder-id)))))
      (mapcar #'content->image-dto
              (cocoa.folder:folder-contents
               folder folder-repos :from from :size size)))))


;;;; Add

;;; The representation of the source of a folder
(defstruct source name modified-at thumbnail contents)
(export 'make-source)

@export
(defun add-bulk (folder-repos name->folder-id)
  (let ((source->folder-id (alexandria:compose name->folder-id
                                               #'source-name)))
    (lambda (sources)
      (let ((folder-ids (mapcar source->folder-id sources)))
        (let ((folder-configs
               (mapcar (lambda (folder-id source)
                         (cocoa.folder:make-folder-config
                          :id folder-id
                          :name (source-name source)
                          :thumbnail (source-thumbnail source)
                          :modified-at (source-modified-at source)))
                       folder-ids sources))
              (appending-bulk
               (cocoa.folder:make-appending-bulk
                :appendings
                (mapcar (lambda (folder-id source)
                          (cocoa.folder:make-appending
                           :folder-id folder-id
                           :contents (source-contents source)))
                        folder-ids sources))))
          (-> folder-repos
              (cocoa.folder:save-folders folder-configs)
              (cocoa.folder:update-contents appending-bulk)))))))

@export
(defun add (folder-repos name->folder-id)
  (lambda (&key name thumbnail)
    (let ((id (funcall name->folder-id name)))
      (cocoa.folder:save-folders folder-repos
       (list (cocoa.folder:make-folder-config
              :id id
              :name name
              :thumbnail thumbnail
              :modified-at (get-universal-time))))
      (funcall (get-by-id folder-repos) id))))


;;;; Update
@export
(defun change-thumbnail (folder-repos)
  (lambda (folder-id image-id)
    (let ((folder (car (cocoa.folder:load-folders-by-ids
                        folder-repos
                        (list folder-id)))))
      (setf (cocoa.folder:folder-thumbnail folder)
            (cocoa.use-case.folder.thumbnail:make-of-image image-id))
      (cocoa.folder:update-folder folder-repos folder))))

@export
(defun append-contents (folder-repos)
  (lambda (&key folder-id contents)
    (let ((appending (cocoa.folder:make-appending
                      :folder-id folder-id :contents contents)))
      (cocoa.folder:update-contents folder-repos appending))))

@export
(defun delete-by-id (folder-repos)
  (lambda (folder-id)
    (cocoa.folder:delete-folders-by-ids folder-repos (list folder-id))))
