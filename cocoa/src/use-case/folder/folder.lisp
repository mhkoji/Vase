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
  (list :id (cocoa.entity.folder:folder-id folder)
        :name (cocoa.entity.folder:folder-name folder)
        :thumbnail (thumbnail->dto
                    (cocoa.entity.folder:folder-thumbnail folder))))

@export
(defun list-by-range (folder-dao)
  (lambda (&key from size)
    (->> (cocoa.entity.folder:list-by-range folder-dao from size)
         (mapcar #'folder->dto))))

@export
(defun list-by-ids (folder-dao)
  (lambda (ids)
    (->> (cocoa.entity.folder:list-by-ids folder-dao ids)
         (mapcar #'folder->dto))))

@export
(defun search-by-name (folder-dao)
  (lambda (name)
    (->> (cocoa.entity.folder:search-by-name folder-dao name)
         (mapcar #'folder->dto))))


@export
(defun get-by-id (folder-dao)
  (lambda (id)
    (->> (car (cocoa.entity.folder:list-by-ids folder-dao (list id)))
         folder->dto)))


;;;; Get contents

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

@export
(defun list-images (folder-dao)
  ;@type! folder-dao !folder-dao
  (lambda (folder-id &key from size)
    "The use case of listing images in a folder"
    ;@type! folder-id !integer
    ;@type! from integer 0
    ;@type! size integer 100
    (ensure-integer! from 0)
    (ensure-integer! size 100)
    (let ((folder (car (cocoa.entity.folder:list-by-ids
                        folder-dao
                        (list folder-id)))))
      (mapcar #'content->image-dto
              (cocoa.entity.folder:folder-contents
               folder-dao folder :from from :size size)))))


;;;; Add

;;; The representation of the source of a folder
(defstruct source name modified-at thumbnail contents)
(export 'make-source)

@export
(defun add-bulk (folder-dao name->folder-id)
  (lambda (sources)
    (let ((folder-ids (mapcar (alexandria:compose name->folder-id
                                                  #'source-name)
                              sources)))
      (-> folder-dao
          (cocoa.entity.folder:add-all
           (mapcar (lambda (folder-id source)
                     (cocoa.entity.folder:make-folder-config
                      :id folder-id
                      :name (source-name source)
                      :thumbnail (source-thumbnail source)
                      :modified-at (source-modified-at source)))
                   folder-ids sources))
          (cocoa.entity.folder:update-contents
           (cocoa.entity.folder:make-appending-bulk
            :appendings
            (mapcar (lambda (folder-id source)
                      (cocoa.entity.folder:make-appending
                       :folder-id folder-id
                       :contents (source-contents source)))
                    folder-ids sources)))))))

@export
(defun add (folder-dao name->folder-id)
  (lambda (&key name thumbnail)
    (let ((id (funcall name->folder-id name)))
      (cocoa.entity.folder:add-all folder-dao
       (list (cocoa.entity.folder:make-folder-config
              :id id
              :name name
              :thumbnail thumbnail
              :modified-at (get-universal-time))))
      (funcall (get-by-id folder-dao) id))))


;;;; Update
@export
(defun change-thumbnail (folder-dao)
  (lambda (folder-id image-id)
    (let ((folder (car (cocoa.entity.folder:list-by-ids
                        folder-dao
                        (list folder-id)))))
      (setf (cocoa.entity.folder:folder-thumbnail folder)
            (cocoa.use-case.folder.thumbnail:make-of-image image-id))
      (cocoa.entity.folder:update folder-dao folder))))

@export
(defun append-contents (folder-dao)
  (lambda (&key folder-id contents)
    (let ((appending (cocoa.entity.folder:make-appending
                      :folder-id folder-id :contents contents)))
      (cocoa.entity.folder:update-contents folder-dao appending))))

@export
(defun delete-by-id (folder-dao)
  (lambda (folder-id)
    (cocoa.entity.folder:delete-by-ids folder-dao (list folder-id))))
