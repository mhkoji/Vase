(defpackage :vase.db
  (:use :cl)
  (:export :make-db))
(in-package :vase.db)

(defun make-db ()
  (let ((folders nil)
        (tag-edit nil))
    (lambda (key &rest args)
      (ecase key
        (:folder.load-all
         folders)
        (:folder.save-all
         (setf folders (car args)))
        (:tag-edit.load
         tag-edit)
        (:tag-edit.save
         (setf tag-edit (car args)))))))
