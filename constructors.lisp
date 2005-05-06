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

;___________________ constructors _______________________________

(defmacro c-lambda (&body body)
  `(c-lambda-var (slot-c) ,@body))

(defmacro c-lambda-var ((c) &body body)
  `(lambda (,c &aux (self (c-model ,c))
             (.cache (c-value ,c)))
     (declare (ignorable .cache self))
     ,@body))

;-----------------------------------------

(defmacro c? (&body body)
  `(make-c-dependent
    :code ',body
    :value-state :unevaluated
    :rule (c-lambda ,@body)))

(defmacro c?8 (&body body)
  `(make-c-dependent
    :code ',body
    :cyclicp t
    :value-state :unevaluated
    :rule (c-lambda ,@body)))

(defmacro c?dbg (&body body)
  `(make-c-dependent
    :code ',body
    :value-state :unevaluated
    :debug t
    :rule (c-lambda ,@body)))

(defmacro c?_ (&body body)
  `(make-c-dependent
    :code ',body
    :value-state :unevaluated
    :lazy t
    :rule (c-lambda ,@body)))

(defmacro c?? ((&key (tagp nil) (in nil) (trigger nil) (out t))&body body)
  (let ((result (copy-symbol 'result))
        (thetag (gensym)))
     `(make-c-dependent
       :code ',body
       :value-state :unevaluated
       :rule (c-lambda
              (let ((,thetag (gensym "tag"))
                    (*trcdepth* (1+ *trcdepth*))
                    )
                (declare (ignorable self ,thetag))
                ,(when in
                   `(trc "c??> entry" (c-slot-name c) (c-model c) (when ,tagp ,thetag)))
                ,(when trigger `(trc "c??> trigger" .cause c))
                (count-it :c?? (c-slot-name c) (md-name (c-model c)))
                (let ((,result (progn ,@body)))
                  ,(when out `(trc "c?? result:" ,result (c-slot-name c) (when ,tagp ,thetag)))
                  ,result))))))

(defmacro c-formula ((&rest keys &key lazy) &body forms)
  (assert (member lazy '(nil t :once-asked :until-asked :always)))
  `(make-c-dependent
    :code ',forms
    :value-state :unevaluated
    :rule (c-lambda ,@forms)
    ,@keys))

(defmacro c-input ((&rest keys) &optional (value nil valued-p))
  `(make-cell
    :inputp t
    :value-state ,(if valued-p :valid :unbound)
    :value ,value
    ,@keys))

(defmacro c-in (value)
  `(make-cell
    :inputp t
    :value-state :valid
    :value ,value))

(defmacro c-in8 (value)
  `(make-cell
    :inputp t
    :cyclicp t
    :value-state :valid
    :value ,value))

(defmacro c-input-dbg (&optional (value nil valued-p))
  `(make-cell
    :inputp t
    :debug t
    :value-state ,(if valued-p :valid :unbound)
    :value ,value))

(defmacro c... ((value) &body body)
  `(make-c-drifter
    :code ',body
    :value-state :valid
    :value ,value
    :rule (c-lambda ,@body)))

(defmacro c-abs (value &body body)
  `(make-c-drifter-absolute
    :code ',body
    :value-state :valid
    :value ,value
    :rule (c-lambda ,@body)))


(defmacro c-envalue (&body body)
  `(make-c-envaluer
    :envalue-rule (c-lambda ,@body)))

