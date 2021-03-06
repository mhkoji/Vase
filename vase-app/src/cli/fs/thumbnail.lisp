(defpackage :vase.app.cli.fs.thumbnail
  (:use :cl)
  (:import-from :vase.app.cli.fs
                :ensure-thumbnail-exists))
(in-package :vase.app.cli.fs.thumbnail)

;; thumbnail
(defun resize-image (target source x y)
  (let ((resize (format nil "~Ax~A" x y)))
    ;; call ImageMagic
    #+sbcl
    (sb-ext:run-program "/usr/bin/convert"
                        (list source "-resize" resize target)
                        :output t :error t)
    #+ccl
    (ccl:run-program "/usr/bin/convert"
                     (list source "-resize" resize target)
                     :output t :error t)))

(defun create-thumbnail (thumbnail source)
  (resize-image thumbnail source 300 300))

(defun ensure-thumbnail-exists (thumbnail-path source-path)
  (setq source-path (namestring source-path))
  (setq thumbnail-path (namestring thumbnail-path))
  (unless (cl-fad:file-exists-p thumbnail-path)
    (create-thumbnail thumbnail-path source-path))
  thumbnail-path)
