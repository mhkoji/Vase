(defpackage :vase.t.scenario.folder
  (:use :cl :vase.folder)
  (:import-from :cl-arrows :->))
(in-package :vase.t.scenario.folder)

(defun make-thumbnail (thumbnail-id)
  (make-instance 'thumbnail
   :get-id (lambda (this)
             (declare (ignore this))
             thumbnail-id)))

(defmacro load-the-added-folder (db &key test)
  `(let ((thumbnail (make-thumbnail "/f1/aaa:thumb")))
     (bulk-save ,db
                (bulk-create
                 (lambda (str) (format nil "id:~A" str))
                 (list (make-source :name "f1"
                                    :thumbnail thumbnail
                                    :modified-at 100)
                       (make-source :name "f2"
                                    :thumbnail
                                    (make-thumbnail "/f2/bbb:thumb")
                                    :modified-at 200))))
     (let ((folder (vase.folder:load-by-id
                    (vase.folder.repos:make-repository
                     :db ,db
                     :thumbnail-repos
                     (lambda (thumbnail-ids)
                       (,test (equal thumbnail-ids '("/f1/aaa:thumb")))
                       (list thumbnail)))
                    "id:f1")))
       (,test (string= (folder-id folder) "id:f1"))
       (,test (eq (folder-thumbnail folder) thumbnail)))))
(export 'load-the-added-folder)
