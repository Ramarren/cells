#| Now we have automatic state management (including change propagation)
outside the Cells model as well as in. Now lets look at cascading change
by adding another level of computation, so A->B->C.

[Actually, I see I need to make this a little deeper, since area has
a direct dependency on width. Not tonight. :)]

|#

(defpackage #:tu-depth (:use :cl :cells))
(in-package #:tu-depth)


(defmodel rectangle ()
  ((area :initarg :area :accessor area
     :initform (c? (print :compue-area)
                 (* (len self)(width self))))
   (len :initarg :len :accessor len
     :initform (c? (print :compute-len)
                 (* 2 (width self))))
   (width :initarg :width :accessor width
     :initform (c? (print :compute-width)
                 (floor (len self) 2)))))

#+test
(let ((r (make-instance 'rectangle :len (c-in 42))))
  (cells::ct-assert (eql 21 (width r)))
  (cells::ct-assert (eql (* 21 42) (area r)))
  (cells::ct-assert (= 1000 (setf (len r) 1000)))
  (cells::ct-assert (eql 500000 (area r))))

