(defpackage :vase.t.scenario.tag
  (:use :cl :vase.tag)
  (:import-from :cl-arrows :->))
(in-package :vase.t.scenario.tag)

(defclass thumbnail ()
  ((id
    :initarg :id
    :reader vase.folder:thumbnail-id)))

(defun make-thumbnail (thumbnail-id)
  (make-instance 'thumbnail :id thumbnail-id))


(defmacro change-the-name-of-a-tag (db &key test)
  `(progn
     (let ((tag (vase.tag.repos:save ,db "tag")))
       (setf (tag-name tag) "tag (updated)")
       (vase.tag.repos:update ,db tag))
     (let ((tag (car (vase.tag.repos:bulk-load-by-range ,db 0 10))))
       (,test (string= (tag-name tag) "tag (updated)")))))
(export 'change-the-name-of-a-tag)


(defmacro delete-a-tag (db &key test)
  `(progn
     (vase.tag.repos:save ,db "tag")
     (let ((tags (vase.tag.repos:bulk-load-by-range ,db 0 10)))
       (,test (= (length tags) 1))
       (,test (string= (-> (car tags) tag-id) "1"))
       (,test (string= (-> (car tags) tag-name) "tag")))

     (vase.tag.repos:bulk-delete ,db (list "1"))
     (,test (null (vase.tag.repos:bulk-load-by-range ,db 0 10)))))
(export 'delete-a-tag)


(defmacro attach-tags-to-a-folder (db &key test)
  `(let ((tag (vase.tag.repos:save ,db "tag"))
         (thumbnail (make-thumbnail "/f1/aaa:thumb")))
     (let ((folders
            (vase.folder:bulk-create
             (lambda (str)
               (format nil "id:~A" str))
             (list (vase.folder:make-source
                    :name "f1"
                    :thumbnail thumbnail
                    :modified-at 100)
                   (vase.folder:make-source
                    :name "f2"
                    :thumbnail (make-thumbnail "/f2/bbb:thumb")
                    :modified-at 200)))))
       (vase.folder:bulk-save ,db folders)

       (let ((content (car folders)))
         (vase.tag.repos:attach-tag ,db tag content)

         (let ((folder-repos
                (vase.folder.repos:make-repository
                 :db ,db
                 :thumbnail-repos
                 (lambda (thumbnail-ids)
                   (,test (equal thumbnail-ids '("/f1/aaa:thumb")))
                   (list thumbnail)))))
           (let ((contents (tag-contents tag ,db folder-repos)))
             (,test (= (length contents) 1))
             (,test (string= (-> (car contents) vase.folder:folder-id)
                             "id:f1"))
             (,test (string= (-> (car contents) vase.folder:folder-name)
                             "f1"))
             (,test (eq (-> (car contents) vase.folder:folder-thumbnail)
                        thumbnail))))
         (let ((tags (vase.tag.repos:bulk-load-by-content ,db content)))
           (,test (= (length tags) 1))
           (,test (string= (-> (car tags) tag-id)
                           "1"))
           (,test (string= (-> (car tags) tag-name)
                           "tag")))))))
(export 'attach-tags-to-a-folder)

(defmacro change-tags-attached-a-folder (db &key test)
  `(let ((tag (vase.tag.repos:save ,db "tag"))
         (thumbnail (make-thumbnail "/f1/aaa:thumb")))
     (let ((folders
            (vase.folder:bulk-create
             (lambda (str)
               (format nil "id:~A" str))
             (list (vase.folder:make-source
                    :name "f1"
                    :thumbnail thumbnail
                    :modified-at 100)))))
       (vase.folder:bulk-save ,db folders)

       (let ((content (car folders)))
         (vase.tag:set-content-tags ,db content (list tag))

         (let ((new-tag (vase.tag.repos:save ,db "New tag")))
           (vase.tag:set-content-tags ,db content (list new-tag))

           (let ((folder-repos
                  (vase.folder.repos:make-repository
                   :db ,db
                   :thumbnail-repos
                   (lambda (thumbnail-ids)
                     (,test (equal thumbnail-ids '("/f1/aaa:thumb")))
                     (list thumbnail)))))
             (let ((contents (tag-contents new-tag ,db folder-repos)))
               (,test (= (length contents) 1))
               (,test (string= (-> (car contents) vase.folder:folder-id)
                               "id:f1"))
               (,test (string= (-> (car contents) vase.folder:folder-name)
                               "f1"))
               (,test (eq (-> (car contents) vase.folder:folder-thumbnail)
                          thumbnail)))))

         (let ((tags (vase.tag.repos:bulk-load-by-content ,db content)))
           (,test (= (length tags) 1))
           (,test (string= (-> (car tags) tag-id)
                           "2"))
           (,test (string= (-> (car tags) tag-name)
                           "New tag")))))))
(export 'change-tags-attached-a-folder)
