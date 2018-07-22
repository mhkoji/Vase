(defpackage :cocoa.web-simple.controller
  (:use :cl)
  (:import-from :cocoa.di.context :with-dao))
(in-package :cocoa.web-simple.controller)
(cl-annot:enable-annot-syntax)

(defmacro with-error-wrapped (&body body)
  `(handler-case (progn ,@body)
     (error (c)
       nil)))

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
(defun bind-app! (app &key context)
  (do-route! ("/folders"
              :method :get
              :out #'cocoa.web-simple.view:folder-list) app
    (with-dao (dao context)
      (cocoa.folder.overview:list-overviews 0 500
       :folder-repository (cocoa.entity.folder:folder-repository dao)))

  (do-route! ("/_i/:id"
              :method :get
              :in ((image-id :param :id))
              :out #'cocoa.web-simple.view:file) app
    (with-dao (dao context)
      (cocoa.image:get-path image-id
       :image-repository (cocoa.fs.image:image-repository dao))))

  app)


@export
(defun bind-resources! (app root)
  (setf (ningle:route app "/resources/*.*" :method :get)
        (lambda (params)
          (cocoa.web-simple.view:file
           (destructuring-bind (path type) (cdr (assoc :splat params))
             (format nil "~A/resources/~A.~A" root path type)))))
  app)
