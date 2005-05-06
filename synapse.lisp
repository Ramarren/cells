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

(defmacro with-synapse (((&rest closure-vars) &key trcp fire-p fire-value) &body body)
  (declare (ignorable trcp))
  (let ((lex-loc-key (gensym "synapse-id")))
    `(let ((synapse (or (cdr (assoc ',lex-loc-key (cd-synapses
                                                  (car *c-calculators*))))
                      (cdar (push (cons ',lex-loc-key
                                   (let (,@closure-vars)
                                     (make-synaptic-ruled slot-c (,fire-p ,fire-value)
                                       ,@body)))
                             (cd-synapses
                              (car *c-calculators*)))))))
       (c-value-ensure-current synapse))))

(defmacro make-synaptic-ruled (syn-user (fire-p fire-value) &body body)
  (let ((new-value (gensym))
        (c-var (gensym)))
    `(make-c-dependent
      :model (c-model ,syn-user)
      :slot-name (intern (conc$ "syn-" (string (c-slot-name ,syn-user))))
      :code ',body
      :synaptic t
      :rule (c-lambda-var (,c-var)
              (let ((,new-value (progn ,@body)))
                (trc nil "generic synaptic rule sees body value" ,c-var ,new-value)
                (if ,(if fire-p `(funcall ,fire-p ,c-var ,new-value) t)
                  (progn
                    (trc nil "Synapse fire YES!!" ,c-var)
                    (funcall ,fire-value ,c-var ,new-value))
                  .cache))))))

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

