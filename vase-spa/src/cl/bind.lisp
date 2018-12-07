;;; What a controller does is:
;;; 1. Receive an input
;;; 2. Prepare objects used for executing the controller task
;;; 3. Execute the task with the input and prepared objects
;;; 4. Send the output of the task in a formatted style
(defpackage :vase.spa.bind
  (:use :cl :vase.spa.json :vase.spa.html)
  (:import-from :cl-arrows :->))
(in-package :vase.spa.bind)
(cl-annot:enable-annot-syntax)

(defun make-json-response (result &optional (success t))
  (let ((resp ningle:*response*))
    (setf (lack.response:response-headers resp)
          (append (lack.response:response-headers resp)
                  (list :content-type "application/json"))))
  (jsown:to-json
   (jsown:new-js
     ("success" (if success :t :f))
     ("result"  (vase.spa.json:convert result)))))

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

@export
(defun bind-api! (app &key conf)
  (do-route! (("/api/folders" (from :query "from")
                              (size :query "size"))) app
    (vase.contexts:with-folder ((repos factory) conf)
      (declare (ignore factory))
      (vase.folder:bulk-load-by-range repos from size)))
  (do-route! (("/api/folder/:id" (folder-id :param :id))) app
    (vase.contexts:with-folder ((repos factory) conf)
      (declare (ignore factory))
      (vase.folder:load-by-id repos folder-id)))
  (do-route! (("/api/folder/:id/images" (folder-id :param :id)
                                        (from :query "from")
                                        (size :query "size"))) app
    (vase.contexts:with-folder ((repos factory) conf)
      (declare (ignore factory))
      (let ((folder (vase.folder:load-by-id repos folder-id)))
        (let ((contents (vase.folder:folder-contents folder
                         :from from
                         :size size)))
          (remove-if-not (lambda (c) (typep c 'vase.image:image))
                         contents)))))
  (do-route! (("/api/folder/:id/tags" (folder-id :param :id))) app
    (let ((folder
           (vase.contexts:with-folder ((repos factory) conf)
             (declare (ignore factory))
             (vase.folder:load-by-id repos folder-id))))
      (vase.contexts:with-tag ((repos) conf)
        (let ((content (vase.tag.contents:from-folder folder)))
          (vase.tag:bulk-load-by-content repos content)))))
  (do-route! (("/api/folder/:id/tags" (folder-id :param :id)
                                      (tag-ids :query "tag_ids"))
              :method :post) app
    (let ((content
           (vase.tag.contents:from-folder
            (vase.contexts:with-folder ((repos factory) conf)
              (declare (ignore factory))
              (vase.folder:load-by-id repos folder-id)))))
      (vase.contexts:with-tag ((repos) conf)
        (vase.tag:set-tags repos content tag-ids))))

  (do-route! (("/api/tags")) app
    (vase.contexts:with-tag ((repos) conf)
      (vase.tag:bulk-load-by-range repos 0 50)))
  (do-route! (("/api/tags" (name :query "name"))
              :method :post) app
    (vase.contexts:with-tag ((repos) conf)
      (vase.tag:save repos name)))

  (do-route! (("/api/tag/:id" (tag-id :param :id))
              :method :delete) app
    (vase.contexts:with-tag ((repos) conf)
      (vase.tag:bulk-delete repos (list tag-id))))
  (do-route! (("/api/tag/:id/folders" (tag-id :param :id))) app
    (vase.contexts:with-tag ((repos) conf)
      (let ((tag (car (vase.tag:bulk-load-by-ids repos (list tag-id)))))
        (vase.tag:tag-contents tag))))
  (do-route! (("/api/tag/:id" (tag-id :param :id)
                              (name :query "name"))
              :method :put) app
    (vase.contexts:with-tag ((repos) conf)
      (let ((tag (car (vase.tag:bulk-load-by-ids repos (list tag-id)))))
        (setf (vase.tag:tag-name tag) name)
        (vase.tag:update tag))))

  (do-route! (("/_i/:id" (image-id :param :id))
              :out make-file-response) app
    (vase.contexts:with-image ((repos factory) conf)
      (declare (ignore factory))
      (vase.image:image-path
       (car (vase.image:bulk-load-by-ids repos (list image-id))))))
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
