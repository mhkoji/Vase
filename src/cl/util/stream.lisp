(defpackage :cocoa.util.stream
  (:use :cl)
  (:export :stream-cons
           :stream-car
           :stream-cdr
           :stream-map
           :stream-remove-if
           :stream-from-list
           :stream-to-list
           :stream-concat
           :stream-map
           :stream-flat-map
           :do-stream))
(in-package :cocoa.util.stream)

(defmacro stream-cons (x y)
  ;; TODO: cache results
  `(cons (lambda () ,x) (lambda () ,y)))

(defun stream-car (stream)
  (when stream
    (funcall (car stream))))

(defun stream-cdr (stream)
  (when stream
    ;; Does not reach the end of the stream
    (funcall (cdr stream))))

(defun stream-from-list (list)
  (when list
    (stream-cons (car list) (stream-from-list (cdr list)))))

(defun stream-to-list (stream &optional n)
  (if n
      (loop repeat n for str = stream then (stream-cdr str)
        while str collect (stream-car str))
      (loop for str = stream then (stream-cdr str)
       while str collect (stream-car str))))

(defun stream-concat (stream1 stream2)
  (if (null stream1)
      stream2
      (stream-cons (stream-car stream1)
                   (stream-concat (stream-cdr stream1) stream2))))

(defun stream-map (fn stream)
  (when stream
    (stream-cons (funcall fn (stream-car stream))
                 (stream-map fn (stream-cdr stream)))))

(defun stream-remove-if (fn stream)
  (when stream
    (let ((x (stream-car stream)))
      (if (funcall fn x)
          (stream-remove-if fn (stream-cdr stream))
          (stream-cons x (stream-remove-if fn (stream-cdr stream)))))))

(defun stream-flat-map (fn stream)
  (when stream
    (stream-concat (funcall fn (stream-car stream))
                   (stream-flat-map fn (stream-cdr stream)))))

(defmacro do-stream ((var stream) &body body)
  (let ((curr (gensym))
        (next (gensym)))
    `(loop for ,curr = ,stream then ,next while ,curr
           for ,next = (stream-cdr ,curr)
       do (let ((,var (stream-car ,curr)))
            ,@body))))
