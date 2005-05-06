;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
;;; Copyright (c) 1995,2003 by Kenneth William Tilton.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy 
;;; of this software and associated documentation files (the "Software"), to deal 
;;; in the Software without restriction, including without limitation the rights 
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell 
;;; copies of the Software, and to permit persons to whom the Software is furnished 
;;; to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in 
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

#| do list

-- can we lose the special handling of the .kids slot?

-- test drifters (and can they be handled without creating a special
subclass for them?)

|#

(eval-when (compile load)
  (proclaim '(optimize (speed 2) (safety 3) (space 1) (debug 3))))

(in-package :cells)

(defparameter *cell-tests* nil)

#+go
(test-cells)

(defun test-cells ()
  (loop for test in (reverse *cell-tests*)
        do (cell-test-init test)
        (funcall test)))

(defun cell-test-init (name)
  (print (make-string 40 :initial-element #\!))
  (print `(starting test ,name))
  (print (make-string 40 :initial-element #\!))
  (cell-reset))

(defmacro def-cell-test (name &rest body)
  `(progn
     (pushnew ',name *cell-tests*)
     (defun ,name ()
       ,@body)))

(defmacro ct-assert (form &rest stuff)
  `(progn
     (print `(attempting ,',form))
    (assert ,form () "Error stuff ~a" (list ,@stuff))))

(defmodel m-null ()
  ((aa :initform nil :cell nil :initarg :aa :accessor aa)))

(def-cell-test m-null
    (let ((m (make-be 'm-null :aa 42)))
      (ct-assert (= 42 (aa m)))
      (ct-assert (= 21 (decf (aa m) 21)))
      (ct-assert (= 21 (aa m)))
      :okay-m-null))

(defmodel m-var ()
  ((m-var-a :initform nil :initarg :m-var-a :accessor m-var-a)
   (m-var-b :initform nil :initarg :m-var-b :accessor m-var-b)))

(def-c-output m-var-b ()
  (print `(output m-var-b ,self ,new-value ,old-value)))

(def-cell-test m-var
  (let ((m (make-be 'm-var :m-var-a (c-in 42) :m-var-b 1951)))
    (ct-assert (= 42 (m-var-a m)))
    (ct-assert (= 21 (decf (m-var-a m) 21)))
    (ct-assert (= 21 (m-var-a m)))
    :okay-m-var))

(defmodel m-var-output ()
  ((cbb :initform nil :initarg :cbb :accessor cbb)
   (aa :cell nil :initform nil :initarg :aa :accessor aa)))

(def-c-output cbb ()
  (trc "output cbb" self)
  (setf (aa self) (- new-value (if old-value-boundp
                                   old-value 0))))

(def-cell-test m-var-output
  (let ((m (make-be 'm-var-output :cbb (c-in 42))))
    (ct-assert (eql 42 (cbb m)))
    (ct-assert (eql 42 (aa m)))
    (ct-assert (eql 27 (decf (cbb m) 15)))
    (ct-assert (eql 27 (cbb m)))
    (ct-assert (eql -15 (aa m)))
    (list :okay-m-var (aa m))))

(defmodel m-var-linearize-setf ()
  ((ccc :initform nil :initarg :ccc :accessor ccc)
   (ddd :initform nil :initarg :ddd :accessor ddd)))

(def-c-output ccc ()
  (with-deference
      (setf (ddd self) (- new-value (if old-value-boundp
                                        old-value 0)))))

(def-cell-test m-var-linearize-setf
  (let ((m (make-be 'm-var-linearize-setf
                    :ccc (c-in 42)
                    :ddd (c-in 1951))))
    
    (ct-assert (= 42 (ccc m)))
    (ct-assert (= 42 (ddd m)))
    (ct-assert (= 27 (decf (ccc m) 15)))
    (ct-assert (= 27 (ccc m)))
    (ct-assert (= -15 (ddd m)))
    :okay-m-var))

;;; -------------------------------------------------------

(defmodel m-ruled ()
  ((eee :initform nil :initarg :eee :accessor eee)
   (fff :initform (c? (floor (^ccc) 2)) :initarg :fff :accessor fff)))

(def-c-output eee ()
  (print `(output> eee ,new-value old ,old-value)))

(def-c-output fff ()
  (print `(output> eee ,new-value old ,old-value)))

(def-cell-test m-ruled
  (let ((m (make-be 'm-ruled
                    :eee (c-in 42)
                    :fff (c? (floor (^eee) 2)))))
    (trc "___Initial TOBE done____________________")
    (print `(pulse ,*data-pulse-id*))
    (ct-assert (= 42 (eee m)))
    (ct-assert (= 21 (fff m)))
    (ct-assert (= 36 (decf (eee m) 6)))
    (print `(pulse ,*data-pulse-id*))
    (ct-assert (= 36 (eee m)))
    (ct-assert (= 18 (fff m)) m)
    :okay-m-ruled))

(defmodel m-worst-case ()
  ((wc-x :accessor wc-x :initform (c-input () 2))
   (wc-a :accessor wc-a :initform (c? (when (oddp (wc-x self))
                                     (wc-c self))))
   (wc-c :accessor wc-c :initform (c? (evenp (wc-x self))))
   (wc-h :accessor wc-h :initform (c? (or (wc-c self)(wc-a self))))))

(def-cell-test m-worst-case
  (let ((m (make-be 'm-worst-case)))
    (trc "___Initial TOBE done____________________")
    (ct-assert (eql t (wc-c m)))
    (ct-assert (eql nil (wc-a m)))
    (ct-assert (eql t (wc-h m)))
    (ct-assert (eql 3 (incf (wc-x m))))))

