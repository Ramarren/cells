#|  A->B->C works. For efficiency, let's have propagation stop if some rule
computes the same value as last time.
|#

(defpackage #:tu-smart-propagation (:use :cl :cells :utils-kt :tu-cells))
(in-package #:tu-smart-propagation)


;;; -----------------------------------------------

(defmodel rectangle ()
  ((padded-width :initarg :padded-width :accessor padded-width
     :initform (c? (compute-log :padded-width)
                 (+ 10 (width self))))
   (len :initarg :len :accessor len
     :initform (c? (compute-log :len)
                 (* 2 (width self))))
   (width :initarg :width :accessor width
     :initform (c? (compute-log :width)
                 (floor (len self) 2)))))

(defobserver width ()
  (assert (not (eql new-value old-value)))
  (TRC "observing width" new-value old-value)
  (compute-log :width-observer))

(defobserver len ()
  (compute-log :len-observer))

#+test
(let* ((r (progn
            (CELLS-RESET)
            (clear-computed)
            (make-instance 'rectangle :len (c-in 42)))))
  (cells::ct-assert (eql 21 (width r)))
  
  ;; first check that setting an input cell does not
  ;; propagate needlessly...
  
  (clear-computed)
  (verify-not-computed :len-observer :width :width-observer :padded-width)
  (setf (len r) 42) ;; n.b. same as existing value, no change
  (cells::ct-assert (eql 21 (width r))) ;; floor truncates
  (verify-not-computed :len-observer :width :width-observer :padded-width)
  
  ;; now check that intermediate computations, when unchanged
  ;; from the preceding computation, does not propagate needlessly...
  
  (clear-computed)
  (setf (len r) 43)
  (cells::ct-assert (eql 21 (width r))) ;; floor truncates
  (verify-computed :len-observer :width)
  (verify-not-computed :width-observer :padded-width)
  
  #| Ok, so the engine runs the width rule, sees that it computes
the same value as before, so does not invoke either the width
observer or recalculation of are. Very efficient. The sanity check
reconfirms that the engine does do that work when necessary.
|# 
  
  (clear-computed)
  (setf (len r) 44)
  (verify-computed :len-observer :width :width-observer :padded-width))
