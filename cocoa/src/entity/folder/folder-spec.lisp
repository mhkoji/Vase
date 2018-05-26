(defpackage :cocoa.entity.folder.folder-spec
  (:use :cl :cocoa.entity.folder))
(in-package :cocoa.entity.folder.folder-spec)
(cl-annot:enable-annot-syntax)

(defun create-rows (from to)
  (loop for i from from to to
        collect (make-folder-row
                 :id i :name (format nil "name-~A" i))))

@export
(defun can-insert-then-select-the-inserted-rows (dao)
  ;; setup
  (setq dao (folder-insert dao (create-rows 0 3)))
  ;; exercise && verify
  (and (equal (folder-select-ids dao 0 2) (list "0" "1"))
       (equal (folder-select-ids dao 2 4) (list "2" "3"))
       (let ((rows (folder-select dao (list "0" "1"))))
         (every (lambda (row i)
                  (and (string= (folder-row-id row)
                                (format nil "~A" i))
                       (string= (folder-row-name row)
                                (format nil "name-~A" i))))
                rows (list 0 1)))))

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
