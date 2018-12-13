(defpackage :vase.t.scenario.folder.repos.db
  (:use :cl :vase.folder.repos.db.folder)
  (:shadowing-import-from :vase.folder.repos.db.folder :delete))
(in-package :vase.t.scenario.folder.repos.db)

(defun create-rows (from to)
  (loop for i from from to to
        collect (make-row :folder-id i
                          :name (format nil "name-~A" i)
                          :modified-at nil)))

(defmacro insert-then-select-the-inserted-rows (db &key test)
  (let ((g (gensym "DB")))
    `(let ((,g ,db))
       ;; setup
       (setq ,g (insert ,g (create-rows 0 3)))
       ;; exercise && verify
       (,test (equal (select-ids ,g 0 2) (list "0" "1")))
       (,test (equal (select-ids ,g 2 4) (list "2" "3")))
       (let ((rows (select ,g (list "0" "1"))))
         (mapc (lambda (row i)
                 (,test (and (string= (row-folder-id row)
                                      (format nil "~A" i))
                             (string= (row-name row)
                                      (format nil "name-~A" i)))))
               rows (list 0 1))))))
(export 'insert-then-select-the-inserted-rows)

(defmacro insert-then-delete-the-inserted-rows (db &key test)
  `(progn
     ;; setup
     (setq ,db (insert ,db (create-rows 0 3)))
     ;; exercise && verify
     (,test (equal (select-ids ,db 0 4) (list "0" "1" "2" "3")))
     (delete ,db (list "1" "2"))
     (,test (equal (select-ids ,db 0 4) (list "0" "3")))))
(export 'insert-then-delete-the-inserted-rows)
