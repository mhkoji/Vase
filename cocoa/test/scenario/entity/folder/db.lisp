(defpackage :cocoa.test.scenario.entity.folder.db
  (:use :cl
        :cocoa.entity.folder
        :cocoa.entity.folder.db)
  (:import-from :cl-arrows :->))
(in-package :cocoa.test.scenario.entity.folder.db)

(defun create-folder-rows (db from to)
  (loop for i from from to to
        collect (folder-row db
                            i
                            (format nil "name-~A" i)
                            nil)))

(defmacro insert-then-select-the-inserted-rows (db &key test)
  (let ((g (gensym "DB")))
    `(let ((,g ,db))
       ;; setup
       (setq ,g (folder-insert ,g (create-folder-rows ,g 0 3)))
       ;; exercise && verify
       (,test (equal (folder-select-ids ,g 0 2) (list "0" "1")))
       (,test (equal (folder-select-ids ,g 2 4) (list "2" "3")))
       (let ((rows (folder-select ,g (list "0" "1"))))
         (mapc (lambda (row i)
                 (,test (and (string= (folder-row-folder-id row)
                                      (format nil "~A" i))
                             (string= (folder-row-name row)
                                      (format nil "name-~A" i)))))
               rows (list 0 1))))))
(export 'insert-then-select-the-inserted-rows)

(defmacro insert-then-delete-the-inserted-rows (db &key test)
  `(progn
     ;; setup
     (setq ,db (folder-insert ,db (create-folder-rows ,db 0 3)))
     ;; exercise && verify
     (,test (equal (folder-select-ids ,db 0 4) (list "0" "1" "2" "3")))
     (folder-delete ,db (list "1" "2"))
     (,test (equal (folder-select-ids ,db 0 4) (list "0" "3")))))
(export 'insert-then-delete-the-inserted-rows)
