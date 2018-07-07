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
(defstruct dir path image-paths modified-at)
(export 'make-dir)

(defun dir-thumbnail (dir &key make-thumbnail-file
                               id-generator
                               image-repository)
  (let ((thumbnail-path (funcall make-thumbnail-file
                                 (car (dir-image-paths dir)))))
    (let ((thumbnail-image (cocoa.fs.image:make-image
                            (cocoa.id:gen id-generator thumbnail-path)
                            thumbnail-path)))
      (cocoa.fs.image:save-images image-repository
                                  (list thumbnail-image))
      (cocoa.use-case.folder.thumbnail:of-image
       (cocoa.fs.image:image-id thumbnail-image)))))

(defun dir-contents (dir &key id-generator image-repository)
  (let ((images (mapcar (lambda (path)
                          (let ((id (cocoa.id:gen id-generator path)))
                            (cocoa.fs.image:make-image id path)))
                        (dir-image-paths dir))))
    (cocoa.fs.image:save-images image-repository images)
    (mapcar (alexandria:compose
             #'cocoa.use-case.folder.content:of-image
             #'cocoa.fs.image:image-id)
            images)))

@export
(defun add-bulk (dirs &key id-generator
                           image-repository
                           folder-repository
                           make-thumbnail-file)
  (labels ((dir-folder (dir)
             (cocoa.folder:make-folder
              :id (cocoa.id:gen id-generator (dir-path dir))
              :name (dir-path dir)
              :thumbnail (dir-thumbnail dir
                          :make-thumbnail-file make-thumbnail-file
                          :id-generator id-generator
                          :image-repository image-repository)
              :modified-at (dir-modified-at dir))))
    (let ((folders
           (mapcar #'dir-folder dirs))
          (contents-list
           (mapcar (alexandria:rcurry #'dir-contents
                    :id-generator id-generator
                    :image-repository image-repository)
                   dirs)))
      (let ((appending-bulk
             (cocoa.folder:make-appending-bulk
              :appendings (mapcar (lambda (folder contents)
                                    (cocoa.folder:make-appending
                                     :folder folder
                                     :contents contents))
                                  folders contents-list))))
        (cocoa.folder:save-folders folder-repository folders)
        (cocoa.folder:update-contents folder-repository appending-bulk))
      (mapcar #'folder->resp folders))))

(defun add (&key name thumbnail
                 folder-repository
                 id-generator)
  (let ((folder (cocoa.folder:make-folder
                 :id (cocoa.id:gen id-generator name)
                 :name name
                 :thumbnail thumbnail
                 :modified-at (get-universal-time))))
    (cocoa.folder:save-folders folder-repository (list folder))
    (folder->resp folder)))



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
  (let ((folder (car (cocoa.folder:load-folders-by-ids folder-repository
                      (list folder-id)))))
    (let ((appending (cocoa.folder:make-appending
                      :folder folder
                      :contents contents)))
      (cocoa.folder:update-contents folder-repository appending))))

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
