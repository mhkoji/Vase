;;; What a controller does is:
;;; 1. Receive an input
;;; 2. Prepare objects used for executing the controller task
;;; 3. Execute the task with the input and prepared objects
;;; 4. Send the output of the task in a formatted style
(defpackage :cocoa.web.bind
  (:use :cl :cocoa.web.json :cocoa.web.html)
  (:import-from :cocoa.di.context
                :with-dao)
  (:import-from :cl-arrows :->))
(in-package :cocoa.web.bind)
(cl-annot:enable-annot-syntax)

(defun make-json-response (result &optional (success t))
  (let ((resp ningle:*response*))
    (setf (lack.response:response-headers resp)
          (append (lack.response:response-headers resp)
                  (list :content-type "application/json"))))
  (jsown:to-json
   (jsown:new-js
     ("success" (if success :t :f))
     ("result"  result))))

(defun make-file-response (path)
  (funcall (lack.app.file:make-app :file path :root "/") nil))

(defmacro with-error-wrapped (&body body)
  `(handler-case (progn ,@body)
     (error (c)
       (make-json-response (format nil "~A" c) nil))))


(defun query (params q)
  (cdr (assoc q params :test #'string=)))

(defun param (params key)
  (cdr (assoc key params)))

(defmacro with-inputs (clauses params &body body)
  `(let ,(mapcar (lambda (clause)
                   (destructuring-bind (var type key) clause
                     (ecase type
                       (:query
                        `(,var (query ,params ,key)))
                       (:param
                        `(,var (param ,params ,key))))))
                 clauses)
     ,@body))

(defmacro do-route! ((path &key method in out) app &body body)
  `(setf (ningle:route ,app ,path :method ,method)
         (lambda (params)
           (declare (ignorable params))
           (with-error-wrapped
             (with-inputs ,in params
               (funcall ,(or out #'identity) (progn ,@body)))))))
@export
(defun bind-api! (app &key context)
  (do-route! ("/api/folders"
              :method :get
              :in ((from :query "from")
                   (size :query "size"))
              :out (lambda (xs) (make-json-response
                                 (array-of #'folder xs)))) app
    (with-dao (dao context)
      (cocoa.folder:list-folder-overviews from size
       :folder-repository (cocoa.entity.folder:folder-repository dao))))
  (do-route! ("/api/folder/:id"
              :method :get
              :in ((folder-id :param :id))
              :out (lambda (f) (make-json-response (folder f)))) app
    (with-dao (dao context)
      (cocoa.folder:get-folder folder-id
       :folder-repository (cocoa.entity.folder:folder-repository dao))))
  (do-route! ("/api/folder/:id/images"
              :method :get
              :in ((folder-id :param :id)
                   (from :query "from")
                   (size :query "size"))
              :out (lambda (xs) (make-json-response
                                 (array-of #'image xs)))) app
    (with-dao (dao context)
      (cocoa.folder.content:get-images
       folder-id :from from :size size
       :folder-repository
       (cocoa.entity.folder:folder-repository dao)
       :folder-content-repository
       (cocoa.entity.folder:folder-content-repository dao))))
  (do-route! ("/api/folder/:id/tags"
              :method :get
              :in ((folder-id :param :id))
              :out (lambda (xs) (make-json-response
                                 (array-of #'tag xs)))) app
    (with-dao (dao context)
      (cocoa.folder:get-folder-tags folder-id
       :tag-repository (cocoa.entity.tag:tag-repository dao))))
  (do-route! ("/api/folder/:id/tags"
              :method :post
              :in ((folder-id :param :id)
                   (tag-ids :query "tag_ids"))
              :out #'make-json-response) app
    (with-dao (dao context)
      (cocoa.folder:set-folder-tags folder-id tag-ids
       :tag-repository (cocoa.entity.tag:tag-repository dao))))

  (do-route! ("/api/tags"
              :method :get
              :out (lambda (xs) (make-json-response
                                 (array-of #'tag xs)))) app
    (with-dao (dao context)
      (cocoa.tag:list-by-range 0 50
       :tag-repository (cocoa.entity.tag:tag-repository dao))))
  (do-route! ("/api/tags"
              :method :post
              :in ((name :query "name"))
              :out #'make-json-response) app
    (with-dao (dao context)
      (cocoa.tag:create name
       :tag-repository (cocoa.entity.tag:tag-repository dao))))
  (do-route! ("/api/tag/:id"
              :method :delete
              :in ((tag-id :param :id))
              :out #'make-json-response) app
    (with-dao (dao context)
      (cocoa.tag:delete-by-id tag-id
       :tag-repository (cocoa.entity.tag:tag-repository dao))))
  (do-route! ("/api/tag/:id/folders"
              :method :get
              :in ((tag-id :param :id))
              :out (lambda (xs) (make-json-response
                                 (array-of #'folder xs)))) app
    (with-dao (dao context)
      (cocoa.tag.contents:get-folders tag-id
       :tag-repository (cocoa.entity.tag:tag-repository dao)
       :folder-repository (cocoa.entity.folder:folder-repository dao))))
  (do-route! ("/api/tag/:id"
              :method :put
              :in ((tag-id :param :id)
                   (name :query "name"))
              :out #'make-json-response) app
    (with-dao (dao context)
      (cocoa.tag:change-name tag-id name
       :tag-repository (cocoa.entity.tag:tag-repository dao))))

  (do-route! ("/_i/:id"
              :method :get
              :in ((image-id :param :id))
              :out #'make-file-response) app
    (with-dao (dao context)
      (cocoa.image:get-path image-id
       :image-repository (cocoa.entity.fs.image:image-repository dao))))
  app)

@export
(defun bind-html! (app)
  (setf (ningle:route app "/.*" :method :get :regexp t)
        (lambda (params)
          (declare (ignore params))
          (main-html "/resources/compiled/cljs/bundle.js")))
  app)

@export
(defun bind-resources! (app root)
  (setf (ningle:route app "/resources/*.*" :method :get)
        (lambda (params)
          (make-file-response
           (destructuring-bind (path type) (cdr (assoc :splat params))
             (format nil "~A/resources/~A.~A" root path type)))))
  app)
