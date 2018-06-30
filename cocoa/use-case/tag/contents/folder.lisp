(defpackage :cocoa.use-case.tag.contents.folder
  (:use :cl :cocoa.use-case.tag.contents))
(in-package :cocoa.use-case.tag.contents.folder)
(cl-annot:enable-annot-syntax)

(defun as-content (folder-id)
  (make-instance 'cocoa.tag:content :id folder-id :type :folder))

@export
(defclass container ()
  ((list-folders-by-ids :initarg :list-folders-by-ids)))

(defmethod list-typed-contents ((container container)
                                (type (eql :folder))
                                (content-ids list))
  (funcall (slot-value container 'list-folders-by-ids) content-ids))


@export
(defun set-tags! (tag-repository)
  (lambda (folder-id tag-ids)
    (let ((content (as-content folder-id)))
      (dolist (tag (cocoa.tag:load-tags-by-content tag-repository content))
        (cocoa.tag:detach-tag tag content))
      (dolist (tag (cocoa.tag:load-tags-by-ids tag-repository tag-ids))
        (cocoa.tag:attach-tag tag content)))))

(defun accept-folder-id (folder-id)
  (assert (typep folder-id 'string))
  folder-id)

@export
(defun get-tags (tag-repository)
  (let ((list-by-content (cocoa.use-case.tag:list-by-content
                          tag-repository)))
    (alexandria:compose list-by-content
                        #'as-content
                        #'accept-folder-id)))
