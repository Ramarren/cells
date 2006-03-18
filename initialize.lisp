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

(eval-when (compile eval load)
  (export '(c-envalue)))

(defstruct (c-envaluer (:conc-name nil))
  envalue-rule)


(defmethod c-awaken-cell (c)
  (declare (ignorable c)))

(defmethod c-awaken-cell ((c cell))
  (assert (c-inputp c))
  #+goforit(when (and (c-ephemeral-p c)
          (c-value c))
    (c-break "Feature not yet supported: initializing ephemeral to other than nil: [~a]"
      (c-value c)))
  ;
  ; nothing to calculate, but every cellular slot should be output
  ;
  (slot-value-observe (c-slot-name c) (c-model c) (c-value c) nil nil)
  (c-ephemeral-reset c))

(defmethod c-awaken-cell ((c c-ruled))
  (let (*c-calculators*)
    (c-calculate-and-set c)))

#+cormanlisp ; satisfy CormanCL bug
(defmethod c-awaken-cell ((c c-dependent))
  (let (*c-calculators*)
    (trc nil "c-awaken-cell c-dependent clearing *c-calculators*" c)
    (c-calculate-and-set c)))

(defmethod c-awaken-cell ((c c-drifter))
  ;
  ; drifters *begin* valid, so the derived version's test for unbounditude
  ; would keep (drift) rule ever from being evaluated. correct solution
  ; (for another day) is to separate awakening (ie, linking to independent
  ; cs) from evaluation, tho also evaluating if necessary during
  ; awakening, because awakening's other role is to get an instance up to speed
  ; at once upon instantiation 
  ;
  (c-calculate-and-set c)
  (cond ((c-validp c) (c-value c))
        ((c-unboundp c) nil)
        (t "illegal state!!!")))
