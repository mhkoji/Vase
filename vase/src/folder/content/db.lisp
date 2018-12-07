(defpackage :vase.folder.content.db
  (:use :cl)
  (:export :content
           :content-type
           :content-entity-id
           :make-appending
           :bulk-append
           :bulk-load
           :bulk-delete))
(in-package :vase.folder.content.db)

(defclass content ()
  ((type :initarg :type
         :type keyword
         :reader content-type)
   (get-entity-id :initarg :get-entity-id
                  :reader get-entity-id)))

(defun content-entity-id (c)
  (funcall (get-entity-id c) c))


(defun content-id (content)
  (let ((type (string-downcase
               (symbol-name
                (content-type content))))
        (entity-id (content-entity-id content)))
    (format nil "~A:~A" type entity-id)))

(defun content-id->type (content-id)
  (alexandria:make-keyword
   (string-upcase
    (first (cl-ppcre:split ":" content-id)))))

(defun content-id->entity-id (content-id)
  (second (cl-ppcre:split ":" content-id)))


(defstruct appending
  folder-id contents)

(defun bulk-append (db appendings)
  (dolist (appending appendings)
    (vase.db.folder.content:insert
     db
     (appending-folder-id appending)
     (mapcar #'content-id (appending-contents appending)))))


(defun safe-subseq (seq from size)
  (let* ((start (or from 0))
         (end (when (numberp size)
                (min (length seq) (+ size start)))))
    (subseq seq start end)))


(defun bulk-load (db content-repos folder-id &key from size)
  (let ((all-content-ids (vase.db.folder.content:select-content-ids
                          db
                          folder-id)))
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
                     (add-contents (vase.folder.content.repos:bulk-load
                                    content-repos
                                    type
                                    entity-ids)))
                   type->entity-ids)))
      (mapcar (lambda (content-id)
                (gethash content-id content-id->content))
              content-ids))))


(defun bulk-delete (db folder-ids)
  (vase.db.folder.content:delete db folder-ids))
