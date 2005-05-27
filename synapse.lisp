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

(in-package :cells)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(mk-synapse f-delta f-sensitivity f-plusp f-zerop fdifferent)))

(defmacro with-synapse (synapse-id (&rest closure-vars) &body body)
  (declare (ignorable trcp))
  (let ((syn-id (gensym))(syn-user (gensym)))
    `(let* ((,syn-id (eko ("!!! syn-id =") ,synapse-id))
            (,syn-user (car *c-calculators*))
            (synapse (or (find ,syn-id (cd-useds ,syn-user) :key 'c-slot-name)
                       (let ((new-syn
                              (let (,@closure-vars)
                                (trc "withsyn making new syn" ,syn-id
                                  :known (mapcar 'c-slot-name (cd-useds ,syn-user)))
                                (make-c-dependent
                                 :model (c-model ,syn-user)
                                 :slot-name ,syn-id
                                 :code ',body
                                 :synaptic t
                                 :rule (c-lambda ,@body)))))
                         (c-link-ex new-syn)
                         new-syn))))
       (prog1
           (with-integrity (:with-synapse)
             (c-value-ensure-current synapse))
         (c-link-ex synapse)))))


;__________________________________________________________________________________
;

(defmethod delta-exceeds (bool-delta sensitivity (subtypename (eql 'boolean)))
  (unless (eql bool-delta :unchanged)
    (or (eq sensitivity t)
        (eq sensitivity bool-delta))))

(defmethod delta-diff ((new number) (old number) subtypename)
  (declare (ignore subtypename))
  (- new old))

(defmethod delta-identity ((dispatcher number) subtypename)
  (declare (ignore subtypename))
  0)

(defmethod delta-abs ((n number) subtypename)
  (declare (ignore subtypename))
  (abs n))

(defmethod delta-exceeds ((d1 number) (d2 number) subtypename)
  (declare (ignore subtypename))
  (> d1 d2))

(defmethod delta-greater-or-equal ((d1 number) (d2 number) subtypename)
  (declare (ignore subtypename))
  (>= d1 d2))

;_________________________________________________________________________________
;
(defmethod delta-diff (new old (subtypename (eql 'boolean)))
   (if new
       (if old
           :unchanged
         :on)
     (if old
         :off
       :unchanged)))


(defmethod delta-identity (dispatcher (subtypename (eql 'boolean)))
   (declare (ignore dispatcher))
   :unchanged)

