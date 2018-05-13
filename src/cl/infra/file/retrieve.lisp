(defpackage :cocoa.infra.file.retrieve
  (:use :cl :cocoa.util.stream))
(in-package :cocoa.infra.file.retrieve)
(cl-annot:enable-annot-syntax)

(defun files-and-dirs (dir sort-files)
  (let ((files nil)
        (sub-directories nil))
    (dolist (child-path (cl-fad:list-directory dir))
      (if (cl-fad:directory-pathname-p child-path)
          (push child-path sub-directories)
          (push (namestring child-path) files)))
    (list (funcall sort-files (nreverse files)) sub-directories)))

@export
(defun retrieve (root &key (sort-files #'identity) make-thumbnail)
  (destructuring-bind (files sub-dirs) (files-and-dirs root sort-files)
    (stream-concat
     (when files
       (stream-from-list
        (list (list :path (namestring root)
                    :files (mapcar #'namestring files)
                    :thumbnail-file
                    (funcall make-thumbnail (car files))))))
     (stream-flat-map (lambda (sub-dir)
                        (retrieve sub-dir
                         :sort-files sort-files
                         :make-thumbnail make-thumbnail))
                      (stream-from-list sub-dirs)))))
