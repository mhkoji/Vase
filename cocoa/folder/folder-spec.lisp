(defpackage :cocoa.folder.folder-spec
  (:use :cl :cocoa.folder)
  (:import-from :cl-arrows :->))
(in-package :cocoa.folder.folder-spec)
(cl-annot:enable-annot-syntax)

(defun create-configs (from to)
  (loop for i from from to to
        collect (make-folder-config
                 :id i
                 :name (format nil "name-~A" i))))

@export
(defmacro can-insert-then-select-the-inserted-rows (dao &key test)
  (let ((g (gensym "DAO")))
    `(let ((,g ,dao))
       ;; setup
       (setq ,g (folder-insert ,g (create-configs 0 3)))
       ;; exercise && verify
       (,test (equal (folder-select-ids ,g 0 2) (list "0" "1")))
       (,test (equal (folder-select-ids ,g 2 4) (list "2" "3")))
       (let ((rows (folder-select ,g (list "0" "1"))))
         (every (lambda (row i)
                  (,test (and (string= (folder-row-folder-id row)
                                       (format nil "~A" i))
                              (string= (folder-row-name row)
                                       (format nil "name-~A" i)))))
                rows (list 0 1))))))

@export
(defun can-insert-then-delete-the-inserted-rows (dao)
  ;; setup
  (setq dao (folder-insert dao (create-configs 0 3)))
  ;; exercise && verify
  (and (equal (folder-select-ids dao 0 4) (list "0" "1" "2" "3"))
       (progn
         (folder-delete dao (list "1" "2"))
         t)
       (equal (folder-select-ids dao 0 4) (list "0" "3"))))

@export
(defun folder-can-contain-contents (folder-repository)
  (let ((folder-id "1234")
        (contents (list (make-content "c:5678"))))
    (setq folder-repository
     (-> folder-repository
         (save-folders (list (make-folder-config
                              :id folder-id
                              :name "a folder name"
                              :thumbnail (make-thumbnail "thumb:1234")
                              :modified-at 3736501114)))
         (update-contents (make-appending :folder-id folder-id
                                          :contents contents))))
    (let ((folder (car (load-folders-by-ids folder-repository
                                            (list folder-id)))))
      (every (lambda (folder-content content)
               (string= (content-id folder-content)
                        (content-id content)))
             (folder-contents folder folder-repository
                              :from 0 :size (length contents))
             contents))))
