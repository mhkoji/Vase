(defpackage :vase.cli.add-folders.fs.retrieve
  (:use :cl :vase.cli.add-folders.stream))
(in-package :vase.cli.add-folders.fs.retrieve)
(cl-annot:enable-annot-syntax)

(defun files-and-subdirectories (dir)
  (let ((files nil)
        (subdirectories nil))
    (dolist (child-path (cl-fad:list-directory dir))
      (if (cl-fad:directory-pathname-p child-path)
          (push child-path subdirectories)
          (push (namestring child-path) files)))
    (list (nreverse files) subdirectories)))

@export
(defun retrieve (root)
  (destructuring-bind (files subdirs) (files-and-subdirectories root)
    (stream-concat
     (when files
       (stream-from-list (list (list :path
                                     (namestring root)
                                     :file-paths
                                     (mapcar #'namestring files)))))
     (stream-flat-map #'retrieve (stream-from-list subdirs)))))
