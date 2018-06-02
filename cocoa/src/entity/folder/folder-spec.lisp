(defpackage :cocoa.entity.folder.folder-spec
  (:use :cl :cocoa.entity.folder)
  (:import-from :cl-arrows :->))
(in-package :cocoa.entity.folder.folder-spec)
(cl-annot:enable-annot-syntax)

(defun create-rows (from to)
  (loop for i from from to to
        collect (make-folder-row
                 :id i :name (format nil "name-~A" i))))

@export
(defmacro can-insert-then-select-the-inserted-rows (dao &key test)
  (let ((g (gensym "DAO")))
    `(let ((,g ,dao))
       ;; setup
       (setq ,g (folder-insert ,g (create-rows 0 3)))
       ;; exercise && verify
       (,test (equal (folder-select-ids ,g 0 2) (list "0" "1")))
       (,test (equal (folder-select-ids ,g 2 4) (list "2" "3")))
       (let ((rows (folder-select ,g (list "0" "1"))))
         (every (lambda (row i)
                  (,test (and (string= (folder-row-id row)
                                       (format nil "~A" i))
                              (string= (folder-row-name row)
                                       (format nil "name-~A" i)))))
                rows (list 0 1))))))

@export
(defun can-insert-then-delete-the-inserted-rows (dao)
  ;; setup
  (setq dao (folder-insert dao (create-rows 0 3)))
  ;; exercise && verify
  (and (equal (folder-select-ids dao 0 4) (list "0" "1" "2" "3"))
       (progn
         (folder-delete dao (list "1" "2"))
         t)
       (equal (folder-select-ids dao 0 4) (list "0" "3"))))


(defclass mock-thumbnail ()
  ((thumbnail-id
    :initarg :thumbnail-id
    :reader thumbnail-id)))

(defclass mock-content ()
  ((content-id
    :initarg :content-id
    :reader content-id)))

(defun make-thumbnail (thumbnail-id)
  (make-instance 'mock-thumbnail :thumbnail-id thumbnail-id))

(defun make-content (content-id)
  (make-instance 'mock-content :content-id content-id))

@export
(defun folder-can-contain-contents (dao)
  (let ((folder-id "1234")
        (contents (list (make-content "c:5678"))))
    (-> dao
        (do-add!
          (add! :id folder-id
                :name "a folder name"
                :thumbnail (make-thumbnail "thumb:1234")
                :modified-at 3736501114))
        (do-update!
          (update! folder-id (append-contents contents))))
    (let ((folder (car (list-by-ids dao
                                    (make-list-spec)
                                    (list folder-id)))))
      (every (lambda (folder-content content)
               (string= (content-id folder-content)
                        (content-id content)))
             (list-contents folder :from 0 :size (length contents))
             contents))))
