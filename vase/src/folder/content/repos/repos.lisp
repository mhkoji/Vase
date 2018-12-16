(defpackage :vase.folder.content.repos
  (:use :cl)
  (:import-from :vase.folder.content
                :content-type
                :content-entity-id

                :bulk-delete

                :make-appending
                :bulk-append

                :bulk-load
                :bulk-load-by-folder)
  (:export :folder-id
           :bulk-append
           :make-appending
           :bulk-load-by-folder))
(in-package :vase.folder.content.repos)

(defgeneric folder-id (f))


(defun bulk-delete (db folders)
  (vase.folder.content.repos.db:delete db (mapcar #'folder-id folder-ids)))


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


(defstruct appending folder contents)

(defun bulk-append (db appendings)
  (dolist (appending appendings)
    (let ((folder-id (folder-id (appending-folder appending)))
          (content-ids (mapcar #'content-id
                               (appending-contents appending))))
      (vase.folder.content.repos.db:insert db folder-id content-ids))))


(defun bulk-load-by-content-ids (content-repos content-ids)
  (let ((content-id->content (make-hash-table :test #'equal)))
    (let ((type->entity-ids (make-hash-table :test #'equal)))
      (dolist (content-id content-ids)
        (let ((type (content-id->type content-id))
              (entity-id (content-id->entity-id content-id)))
          (push entity-id (gethash type type->entity-ids))))
      (maphash
       (lambda (type entity-ids)
         (let ((contents (bulk-load content-repos type entity-ids)))
           (dolist (content contents)
             (let ((content-id (content-id content)))
               (setf (gethash content-id content-id->content) content)))))
       type->entity-ids))
    (mapcar (lambda (content-id)
              (gethash content-id content-id->content))
            content-ids)))

(labels ((safe-subseq (seq from size)
           (let* ((start (or from 0))
                  (end (when (numberp size)
                         (min (length seq) (+ size start)))))
             (subseq seq start end))))
  (defun bulk-load-by-folder (content-repos db folder &key from size)
    (let ((all-content-ids (vase.folder.content.repos.db:select-content-ids
                            db
                            (folder-id folder))))
      (bulk-load-by-content-ids content-repos
                                (safe-subseq all-content-ids from size)))))
