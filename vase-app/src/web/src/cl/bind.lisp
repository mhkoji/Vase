;;; What a controller does is:
;;; 1. Receive an input
;;; 2. Prepare objects used for executing the controller task
;;; 3. Execute the task with the input and prepared objects
;;; 4. Send the output of the task in a formatted style
(defpackage :vase.app.web.bind
  (:use :cl
        :vase.app.web.json
        :vase.app.web.html
        :vase.app.container)
  (:export :bind-api!
           :bind-html!
           :bind-resources!))
(in-package :vase.app.web.bind)

(defun make-json-response (result &optional (success t))
  (let ((resp ningle:*response*))
    (setf (lack.response:response-headers resp)
          (append (lack.response:response-headers resp)
                  (list :content-type "application/json"))))
  (jsown:to-json
   (jsown:new-js
     ("success" (if success :t :f))
     ("result"  (vase.app.web.json:convert result)))))

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

(defmacro do-route! (((path &rest in)
                      &key (method :get)
                           (out 'make-json-response)) app &body body)
  `(setf (ningle:route ,app ,path :method ,method)
         (lambda (params)
           (declare (ignorable params))
           (with-error-wrapped
             (with-inputs ,in params
               (funcall #',out (progn ,@body)))))))

(defun bind-api! (app &key conf)
  (do-route! (("/api/folders" (from :query "from")
                              (size :query "size"))) app
    (with-container (c conf)
      (with-accessors ((folder-repos container-folder-repository)) c
        (vase.folder:bulk-load-by-range folder-repos from size))))
  (do-route! (("/api/folder/:id" (folder-id :param :id))) app
    (with-container (c conf)
      (with-accessors ((folder-repos container-folder-repository)) c
        (vase.folder:load-by-id folder-repos folder-id))))
  (do-route! (("/api/folder/:id/images" (folder-id :param :id)
                                        (from :query "from")
                                        (size :query "size"))) app
    (with-container (c conf)
      (with-accessors ((db container-db)
                       (image-repos container-image-repository)
                       (folder-repos container-folder-repository)) c
        (let ((folder (vase.folder:load-by-id folder-repos folder-id)))
          (vase.folder:folder-contents folder db image-repos
                                       :from from
                                       :size size)))))
  (do-route! (("/api/folder/:id/tags" (folder-id :param :id))) app
    (with-container (c conf)
      (with-accessors ((db container-db)
                       (folder-repos container-folder-repository)) c
        (let ((folder (vase.folder:load-by-id folder-repos folder-id)))
          (let ((content (vase.tag.contents:from-folder folder)))
            (vase.tag:content-tags content db))))))
  (do-route! (("/api/folder/:id/tags" (folder-id :param :id)
                                      (tag-ids :query "tag_ids"))
              :method :post) app
    (with-container (c conf)
      (with-accessors ((db container-db)
                       (folder-repos container-folder-repository)) c
        (let ((folder (vase.folder:load-by-id folder-repos folder-id)))
          (let ((tags (vase.tag:bulk-load-by-ids db tag-ids))
                (content (vase.tag.contents:from-folder folder)))
            (vase.tag:set-content-tags db content tags))))))

  (do-route! (("/api/tags")) app
    (with-container (c conf)
      (with-accessors ((db container-db)) c
        (vase.tag:bulk-load-by-range db 0 50))))
  (do-route! (("/api/tags" (name :query "name"))
              :method :post) app
    (with-container (c conf)
      (with-accessors ((db container-db)) c
        (vase.tag:save db name))))

  (do-route! (("/api/tag/:id" (tag-id :param :id))
              :method :delete) app
    (with-container (c conf)
      (with-accessors ((db container-db)) c
        (vase.tag:bulk-delete db (list tag-id)))))
  (do-route! (("/api/tag/:id/folders" (tag-id :param :id))) app
    (with-container (c conf)
      (with-accessors ((db container-db)
                       (folder-repos container-folder-repository)) c
        (let ((tag (car (vase.tag:bulk-load-by-ids db (list tag-id)))))
          (vase.tag:tag-contents tag db folder-repos)))))
  (do-route! (("/api/tag/:id" (tag-id :param :id)
                              (name :query "name"))
              :method :put) app
    (with-container (c conf)
      (with-accessors ((db container-db)) c
        (let ((tag (car (vase.tag:bulk-load-by-ids db (list tag-id)))))
          (setf (vase.tag:tag-name tag) name)
          (vase.tag:update db tag))))
    (values))

  (do-route! (("/_i/:id" (image-id :param :id))
              :out make-file-response) app
    (with-container (c conf)
      (with-accessors ((image-repos container-image-repository)) c
        (let ((image (car (vase.image:bulk-load-by-ids image-repos
                                                       (list image-id)))))
          (vase.image:image-path image)))))
  app)

(defun bind-html! (app)
  (setf (ningle:route app "/.*" :method :get :regexp t)
        (lambda (params)
          (declare (ignore params))
          (main-html "/resources/compiled/cljs/bundle.js")))
  app)

(defun bind-resources! (app root)
  (setf (ningle:route app "/resources/*.*" :method :get)
        (lambda (params)
          (make-file-response
           (destructuring-bind (path type) (cdr (assoc :splat params))
             (format nil "~A/resources/~A.~A" root path type)))))
  app)
