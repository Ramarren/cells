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

(defstruct (cell (:conc-name c-))
  model
  slot-name
  value
  
  inputp ;; t for old c-variable class
  cyclicp ;; t if OK for setf to cycle back (ending cycle)
  synaptic
  changed
  (users nil :type list)
  
  (state :nascent :type symbol) ;; :nascent, :awake, :optimized-away
  (value-state :unbound :type symbol) ;; {:unbound | :unevaluated | :valid}
  (pulse 0 :type fixnum)
  debug
  md-info)

(defun c-unboundp (c)
  (eql :unbound (c-value-state c)))

; -----------------------------------------------------

(defun c-validate (self c)
  (when (not (and (c-slot-name c) (c-model c)))
    (format t "~&unadopted cell: ~s md:~s" c self)
    (c-break "unadopted cell ~a ~a" self c)
    (error 'c-unadopted :cell c)))

(defstruct (c-ruled
            (:include cell)
            (:conc-name cr-))
  lazy
  (code nil :type list) ;; /// feature this out on production build
  rule)

(defun c-optimized-away-p (c)
  (eql :optimized-away (c-state c)))

(defmethod c-lazy ((c c-ruled)) (cr-lazy c))
(defmethod c-lazy (c) (declare (ignore c)) nil)

;----------------------------

(defmethod trcp-slot (self slot-name)
  (declare (ignore self slot-name)))

(define-constant *cd-usagect* 64)

(defstruct (c-dependent
            (:include c-ruled)
            (:conc-name cd-))
  (synapses nil :type list)
  (useds nil :type list)
  (usage (make-array *cd-usagect* :element-type 'bit
                        :initial-element 0) :type vector))

(defstruct (c-stream
            (:include c-dependent)
            (:conc-name cs-))
  values)

(defstruct streamer from stepper donep to)

#+notyet
(defmacro c~~~ (&key (from 0)
                 stepper
                 (donep (c-lambda (> .cache (streamer-to slot-c))))
                 to)
   `(make-c-stream
     :rule (c-lambda (make-streamer
                      :from ,from
                      :stepper ,stepper
                      :to ,to :donep ,donep))))

(defmethod md-slot-value-assume :around ((c c-stream) (s streamer))
  (bif (to (streamer-to s))
    (loop for slot-value = (streamer-from s)
          then (bIf (stepper (streamer-stepper s))
                 (funcall stepper c)
                 (incf slot-value))
          until (bIf (to (streamer-to s))
                  (> slot-value to)
                  (bwhen (donep-test (streamer-donep s))
                    (funcall donep-test c)))
          do (progn
               (print `(assume doing ,slot-value))
               (call-next-method c slot-value))))
  (c-optimize-away?! c))

#+test
(progn
  (defmodel streamertest ()
    ((val :accessor val :initform (c~~~ :from 0 :to (^oval)))
     (oval :initarg :oval :accessor oval :initform (c-in 0))))
  
  (def-c-output val ((self streamertest))
    (print `(streamertest old ,old-value new ,new-value)))
  
  (cell-reset)
  (let ((it (make-be 'streamertest :oval 5)))
    ;;(setf (oval it) 5)
    it))

(defstruct (c-drifter
            (:include c-dependent)))

(defstruct (c-drifter-absolute
            (:include c-drifter)))

;_____________________ accessors __________________________________

(defmethod c-useds (other) (declare (ignore other)))
(defmethod c-useds ((c c-dependent)) (cd-useds c))



(defun c-validp (c)
  (eql (c-value-state c) :valid))

;_____________________ print __________________________________

(defmethod print-object :before ((c cell) stream)
 (declare (ignorable c))
  (format stream "[~a~a:" (if (c-inputp c) "i" "?")
    (cond
     ((null (c-model c)) #\0)
     ((eq :eternal-rest (md-state (c-model c))) #\_)
     ((not (c-currentp c)) #\#)
     (t #\space))))

(defmethod print-object ((c cell) stream)
  (c-print-value c stream)
  (format stream "=[~d]~a/~a]"
    (c-pulse c)
    (symbol-name (or (c-slot-name c) :anoncell))
    (or (c-model c) :anonmd)))

;__________________

(defmethod c-print-value ((c c-ruled) stream)
  (format stream "~a" (cond ((c-validp c) "<vld>")
                            ((c-unboundp c) "<unb>")
                            ((not (c-currentp c)) "<obs>")
                            (t "<err>"))))

(defmethod c-print-value (c stream)
  (declare (ignore c stream)))
