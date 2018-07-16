(defpackage :cocoa.folder
  (:use :cl)
  (:import-from :cl-arrows :-> :->> :-<>))
(in-package :cocoa.folder)
(cl-annot:enable-annot-syntax)

;;;; The definitions of the response objects

(defun content->resp (content)
  (list :id (cocoa.folder.content:content->image-id content)))

(defun thumbnail->resp (thumbnail)
  (list :id (cocoa.folder.thumbnail:thumbnail->image-id thumbnail)))

(defun folder->resp (folder)
  (list :id (cocoa.entity.folder:folder-id folder)
        :name (cocoa.entity.folder:folder-name folder)
        :thumbnail (thumbnail->resp (cocoa.entity.folder:folder-thumbnail
                                     folder))))

(defun tag->resp (tag)
  (list :id (cocoa.entity.tag:tag-id tag)
        :name (cocoa.entity.tag:tag-name tag)))


;;;; List

@export
(defun get-overviews-by-range (from size &key folder-repository)
  (->> (cocoa.entity.folder:load-folders-by-range folder-repository
                                                  from size)
       (mapcar #'folder->resp)))

@export
(defun get-overviews-by-search (name &key folder-repository)
  (->> (cocoa.entity.folder:search-folders-by-name folder-repository
                                                   name)
       (mapcar #'folder->resp)))


;;;; Get

(defun accept-folder-id (folder-id)
  (assert (typep folder-id 'string))
  folder-id)

@export
(defun get-folder (id &key folder-repository)
  (accept-folder-id id)
  (->> (car (cocoa.entity.folder:load-folders-by-ids
             folder-repository
             (list id)))
       folder->resp))

(defmacro ensure-integer! (var default)
  `(progn
     (when (stringp ,var)
       (setq ,var (parse-integer ,var :junk-allowed t)))
     (when (null ,var)
       (setq ,var ,default))))

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
  (let ((folder (car (cocoa.entity.folder:load-folders-by-ids
                      folder-repository
                      (list folder-id)))))
    (->> (cocoa.entity.folder:folder-contents folder-content-repository
          folder :from from :size size)
         (remove-if-not
          #'cocoa.folder.content:content->image-id)
         (mapcar #'content->resp))))

;;;; Update
@export
(defun change-thumbnail (folder-id image-id &key folder-repository)
  (accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder:load-folders-by-ids
                      folder-repository
                      (list folder-id)))))
    (setf (cocoa.entity.folder:folder-thumbnail folder)
          (cocoa.folder.thumbnail:of-image image-id))
    (cocoa.entity.folder:update-folder folder-repository folder)))

@export
(defun append-contents (folder-id contents
                        &key folder-repository
                             folder-content-repository)
  (accept-folder-id folder-id)
  (let ((folder (car (cocoa.entity.folder:load-folders-by-ids
                      folder-repository
                      (list folder-id)))))
    (let ((appending (cocoa.entity.folder:make-appending
                      :folder folder
                      :contents contents)))
      (cocoa.entity.folder:update-contents folder-content-repository
                                           appending))))

@export
(defun delete-by-id (folder-id &key folder-repository)
  (accept-folder-id folder-id)
  (cocoa.entity.folder:delete-folders-by-ids folder-repository
                                             (list folder-id)))


;;;; Add

;;; The representation of a directory in the local file system
(defstruct dir path image-paths modified-at)
(export 'make-dir)

(defun dir-thumbnail (dir &key make-thumbnail-file
                               id-generator
                               image-repository)
  (let ((thumbnail-path (funcall make-thumbnail-file
                                 (car (dir-image-paths dir)))))
    (let ((thumbnail-image (cocoa.entity.fs.image:make-image
                            (cocoa.entity.id:gen id-generator
                                                 thumbnail-path)
                            thumbnail-path)))
      (cocoa.entity.fs.image:save-images image-repository
                                         (list thumbnail-image))
      (cocoa.folder.thumbnail:of-image
       (cocoa.entity.fs.image:image-id thumbnail-image)))))

(defun dir-contents (dir &key id-generator image-repository)
  (let ((images (mapcar (lambda (path)
                          (let ((id (cocoa.entity.id:gen id-generator
                                                         path)))
                            (cocoa.entity.fs.image:make-image id path)))
                        (dir-image-paths dir))))
    (cocoa.entity.fs.image:save-images image-repository images)
    (mapcar (alexandria:compose
             #'cocoa.folder.content:of-image
             #'cocoa.entity.fs.image:image-id)
            images)))

@export
(defun add-bulk (dirs &key id-generator
                           image-repository
                           folder-repository
                           folder-content-repository
                           make-thumbnail-file)
  (labels ((dir-folder (dir)
             (cocoa.entity.folder:make-folder
              :id (cocoa.entity.id:gen id-generator (dir-path dir))
              :name (dir-path dir)
              :thumbnail (dir-thumbnail dir
                          :make-thumbnail-file make-thumbnail-file
                          :id-generator id-generator
                          :image-repository image-repository)
              :modified-at (dir-modified-at dir))))
    (let ((folders (mapcar #'dir-folder dirs)))
      (cocoa.entity.folder:save-folders folder-repository folders)
      (let ((appending-bulk
             (->> dirs
                  (mapcar (alexandria:rcurry #'dir-contents
                           :id-generator id-generator
                           :image-repository image-repository))
                  (mapcar (lambda (folder contents)
                            (cocoa.entity.folder:make-appending
                             :folder folder
                             :contents contents))
                          folders)
                  (cocoa.entity.folder:make-appending-bulk
                   :appendings))))
        (cocoa.entity.folder:update-contents
         folder-content-repository appending-bulk))
      (mapcar #'folder->resp folders))))

(defun add (&key name thumbnail
                 folder-repository
                 id-generator)
  (let ((folder (cocoa.entity.folder:make-folder
                 :id (cocoa.entity.id:gen id-generator name)
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
    (cocoa.entity.folder:save-folders folder-repository (list folder))
    (folder->resp folder)))


;;;; Tag

(defun as-tagged-content (folder-id)
  (accept-folder-id folder-id)
  (make-instance 'cocoa.entity.tag:content :id folder-id :type :folder))

@export
(defclass folder-container ()
  ((folder-repository :initarg :folder-repository)))

(defmethod cocoa.entity.tag:render-contents ((container folder-container)
                                             (type (eql :folder))
                                             (content-ids list))
  (let ((folder-repos (slot-value container 'folder-repository)))
    (->> (cocoa.entity.folder:load-folders-by-ids folder-repos content-ids)
         (mapcar #'folder->resp))))

@export
(defun set-tags (folder-id tag-ids &key tag-repository)
  (let ((content (as-tagged-content folder-id)))
    (dolist (tag (cocoa.entity.tag:load-tags-by-content tag-repository
                                                        content))
      (cocoa.entity.tag:detach-tag tag content))
    (dolist (tag (cocoa.entity.tag:load-tags-by-ids tag-repository
                                                    tag-ids))
      (cocoa.entity.tag:attach-tag tag content))))

@export
(defun get-tags (folder-id &key tag-repository)
  (->> (cocoa.entity.tag:load-tags-by-content
        tag-repository
        (as-tagged-content folder-id))
       (mapcar #'tag->resp)))
