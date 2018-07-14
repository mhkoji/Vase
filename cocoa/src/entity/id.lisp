(defpackage :cocoa.entity.id
  (:use :cl))
(in-package :cocoa.entity.id)
(cl-annot:enable-annot-syntax)

@export
(defgeneric gen (generator string))

@export
(defclass sha256 () ())

(defun sha256 (string)
  (ironclad:byte-array-to-hex-string
   (let ((octets (babel:string-to-octets string :encoding :utf-8)))
     (ironclad:digest-sequence 'ironclad:sha256 octets))))

(defmethod gen ((generator sha256) (string string))
  (sha256 string))


@export
(defclass sha256-3 () ())

(defmethod gen ((generator sha256-3) (string string))
  (subseq (sha256 (sha256 (sha256 string))) 0 10))






