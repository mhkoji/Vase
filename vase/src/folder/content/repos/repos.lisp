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
    (vase.folder.content.repos.db:insert
     db
     (folder-id (appending-folder appending))
     (mapcar #'content-id (appending-contents appending)))))


(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))


(defstruct repository db entity-repos)

(defun bulk-load (repos folder &key from size)
  (let ((all-content-ids (vase.folder.content.repos.db:select-content-ids
                          (repository-db repos)
                          (folder-id folder))))
    (let ((content-ids (safe-subseq all-content-ids from size))
          (content-id->content (make-hash-table :test #'equal)))
      (let ((type->entity-ids (make-hash-table :test #'equal)))
        (dolist (content-id content-ids)
          (let ((type (content-id->type content-id))
                (entity-id (content-id->entity-id content-id)))
            (push entity-id (gethash type type->entity-ids))))
        (labels ((add-contents (contents)
                   (dolist (content contents)
                     (let ((content-id (content-id content)))
                       (setf (gethash content-id content-id->content)
                             content)))))
          (maphash (lambda (type entity-ids)
                     (add-contents
                      (vase.folder.content.entities.repos:bulk-load
                       (repository-entity-repos repos)
                       type
                       entity-ids)))
                   type->entity-ids)))
      (mapcar (lambda (content-id)
                (gethash content-id content-id->content))
              content-ids))))


(defun bulk-delete (db folders)
  (vase.folder.content.repos.db:delete db (mapcar #'folder-id folder-ids)))
