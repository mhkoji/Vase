(defpackage :cocoa.use-case.folder
  (:use :cl)
  (:import-from :cl-arrows :-> :->> :-<>))
(in-package :cocoa.use-case.folder)
(cl-annot:enable-annot-syntax)

;;;; The definitions of the response objects

(defun content->resp (content)
  (list :id (cocoa.use-case.folder.content:content->image-id content)))

(defun thumbnail->resp (thumbnail)
  (list :id (cocoa.use-case.folder.thumbnail:thumbnail->image-id
             thumbnail)))

(defun folder->resp (folder)
  (list :id (cocoa.folder:folder-id folder)
        :name (cocoa.folder:folder-name folder)
        :thumbnail (thumbnail->resp (cocoa.folder:folder-thumbnail folder))))

(defun tag->resp (tag)
  (list :id (cocoa.tag:tag-id tag) :name (cocoa.tag:tag-name tag)))


;;;; Get

@export
(defun list-by-range (from size &key folder-repository)
  (->> (cocoa.folder:load-folders-by-range folder-repository from size)
       (mapcar #'folder->resp)))

@export
(defun list-by-ids (ids &key folder-repository)
  (->> (cocoa.folder:load-folders-by-ids folder-repository ids)
       (mapcar #'folder->resp)))

@export
(defun search-by-name (name &key folder-repository)
  (->> (cocoa.folder:search-folders-by-name folder-repository name)
       (mapcar #'folder->resp)))


@export
(defun get-by-id (id &key folder-repository)
  (->> (car (cocoa.folder:load-folders-by-ids folder-repository (list id)))
       folder->resp))


;;;; Add

;;; The representation of a directory in the local file system
(defstruct dir path file-paths modified-at)
(export 'make-dir)

(defun dir-thumbnail (dir &key make-thumbnail-file add-images-by-paths)
  (let ((thumbnail-file
         (funcall make-thumbnail-file (car (dir-file-paths dir)))))
    (cocoa.use-case.folder.thumbnail:of-image
     (car (funcall add-images-by-paths (list thumbnail-file))))))

(defun dir-folder-config (dir folder-id
                          &key make-thumbnail-file
                               add-images-by-paths)
  (let ((path (dir-path dir)))
    (cocoa.folder:make-folder-config
     :id folder-id
     :name path
     :thumbnail (dir-thumbnail dir
                 :make-thumbnail-file make-thumbnail-file
                 :add-images-by-paths add-images-by-paths)
     :modified-at (dir-modified-at dir))))

(defun dir-appending (dir folder-id &key add-images-by-paths)
  (cocoa.folder:make-appending
   :folder-id folder-id
   :contents (mapcar #'cocoa.use-case.folder.content:of-image
                     (funcall add-images-by-paths (dir-file-paths dir)))))

@export
(defun add-bulk (dirs &key folder-repository
                           make-folder-id-by-path
                           make-thumbnail-file
                           add-images-by-paths)
  (labels ((folder-config (dir folder-id)
             (dir-folder-config dir folder-id
              :make-thumbnail-file make-thumbnail-file
              :add-images-by-paths add-images-by-paths))
           (appending (dir folder-id)
             (dir-appending dir folder-id
              :add-images-by-paths add-images-by-paths))
           (dir-folder-id (dir)
             (funcall make-folder-id-by-path (dir-path dir))))
    (let ((folder-ids (mapcar #'dir-folder-id dirs)))
      (-> folder-repository
          (cocoa.folder:save-folders
           (mapcar #'folder-config dirs folder-ids))
          (cocoa.folder:update-contents
           (cocoa.folder:make-appending-bulk
            :appendings (mapcar #'appending dirs folder-ids)))))))

(defun add (&key name thumbnail
                 folder-repository
                 make-folder-id-by-name)
  (let ((id (funcall make-folder-id-by-name name)))
    (-<> folder-repository
         (cocoa.folder:save-folders
          (list (cocoa.folder:make-folder-config
                 :id id
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
         (get-by-id id :folder-repository <>))))


;;;; Update
@export
(defun change-thumbnail (folder-id image-id &key folder-repository)
  (let ((folder (car (cocoa.folder:load-folders-by-ids folder-repository
                      (list folder-id)))))
    (setf (cocoa.folder:folder-thumbnail folder)
          (cocoa.use-case.folder.thumbnail:of-image image-id))
    (cocoa.folder:update-folder folder-repository folder)))

@export
(defun append-contents (folder-id contents &key folder-repository)
  (let ((appending (cocoa.folder:make-appending
                    :folder-id folder-id :contents contents)))
    (cocoa.folder:update-contents folder-repository appending)))

@export
(defun delete-by-id (folder-id &key folder-repository)
  (cocoa.folder:delete-folders-by-ids folder-repository (list folder-id)))


;;;; Contents

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

@export
(defun get-images (folder-id &key from size folder-repository)
  "The use case of listing images in a folder"
  ;@type! folder-repos !folder-repository
  ;@type! folder-id !integer
  ;@type! from integer 0
  ;@type! size integer 100
  (ensure-integer! from 0)
  (ensure-integer! size 100)
  (let ((folder (car (cocoa.folder:load-folders-by-ids
                      folder-repository
                      (list folder-id)))))
    (->> (cocoa.folder:folder-contents
           folder folder-repository :from from :size size)
         (remove-if-not
          #'cocoa.use-case.folder.content:content->image-id)
         (mapcar #'content->resp))))

;;;; Tag

(defun accept-folder-id (folder-id)
  (assert (typep folder-id 'string))
  folder-id)

(defun as-tagged-content (folder-id)
  (accept-folder-id folder-id)
  (make-instance 'cocoa.tag:content :id folder-id :type :folder))

@export
(defun set-tags (folder-id tag-ids &key tag-repository)
  (let ((content (as-tagged-content folder-id)))
    (dolist (tag (cocoa.tag:load-tags-by-content tag-repository content))
      (cocoa.tag:detach-tag tag content))
    (dolist (tag (cocoa.tag:load-tags-by-ids tag-repository tag-ids))
      (cocoa.tag:attach-tag tag content))))

@export
(defun get-tags (folder-id &key tag-repository)
  (->> (cocoa.tag:load-tags-by-content tag-repository
                                       (as-tagged-content folder-id))
       (mapcar #'tag->resp)))
