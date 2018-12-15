(defpackage :vase.folder.content.repos
  (:use :cl)
  (:export :folder-id
           :content-type
           :content-entity-id

           :make-appending
           :bulk-append
           :make-repository
           :bulk-load
           :bulk-delete))
(in-package :vase.folder.content.repos)

(defgeneric folder-id (f))

(defgeneric content-type (c))
(defgeneric content-entity-id (c))


(defun content-id (content)
  (let ((type (string-downcase
               (symbol-name (content-type content))))
        (entity-id (content-entity-id content)))
    (format nil "~A:~A" type entity-id)))

(defun content-id->type (content-id)
  (alexandria:make-keyword
   (string-upcase
    (first (cl-ppcre:split ":" content-id)))))

(defun content-id->entity-id (content-id)
  (second (cl-ppcre:split ":" content-id)))


(defstruct appending
  folder contents)

(defun bulk-append (db appendings)
  (dolist (appending appendings)
    (let ((folder-id (folder-id (appending-folder appending)))
          (content-ids (mapcar #'content-id
                               (appending-contents appending))))
      (vase.folder.content.repos.db:insert db folder-id content-ids))))

(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))


(defun load-content-ids (db folder &key from size)
  (let ((all-content-ids (vase.folder.content.repos.db:select-content-ids
                          db (folder-id folder))))
    (safe-subseq all-content-ids from size)))

(defun bulk-load (db content-repos folder &key from size)
  (let ((content-ids (load-content-ids db folder
                                       :from from
                                       :size size))
        (content-id->content (make-hash-table :test #'equal)))
    (let ((type->entity-ids (make-hash-table :test #'equal)))
      (dolist (content-id content-ids)
        (let ((type (content-id->type content-id))
              (entity-id (content-id->entity-id content-id)))
          (push entity-id (gethash type type->entity-ids))))
      (maphash
       (lambda (type entity-ids)
         (let ((contents (vase.folder.content:bulk-load content-repos
                                                        type
                                                        entity-ids)))
           (dolist (content contents)
             (let ((content-id (content-id content)))
               (setf (gethash content-id content-id->content) content)))))
       type->entity-ids))
    (mapcar (lambda (content-id)
              (gethash content-id content-id->content))
            content-ids)))


(defun bulk-delete (db folders)
  (vase.folder.content.repos.db:delete db (mapcar #'folder-id folder-ids)))
