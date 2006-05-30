(defpackage #:tu-dataflow (:use :cl :cells))
(in-package #:tu-dataflow)

(defmodel rectangle ()
  ((len :initarg :len :accessor len
     :initform (c? (* 2 (width self))))
   (width :initarg :width :initform nil :accessor width))
  (:default-initargs
      :width (c? (/ (len self) 2))))

#+test
(let ((r (make-instance 'rectangle :len (c-in 42))))
  (cells::ct-assert (eql 21 (width r)))
  (cells::ct-assert (= 1000 (setf (len r) 1000))) ;; make sure we did not break SETF, which must return the value set
  (cells::ct-assert (eql 500 (width r)))) ;; make sure new value propagated


