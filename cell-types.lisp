;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cells)

(defstruct (cell (:conc-name c-))
  model
  slot-name
  value
  
  inputp ;; t for old c-variable class
  synaptic
  changed
  (users-store (make-fifo-queue) :type cons) ;; (C3) probably better to notify users FIFO
  
  (state :nascent :type symbol) ;; :nascent, :awake, :optimized-away
  (value-state :unbound :type symbol) ;; {:unbound | :unevaluated | :valid}
  (pulse 0 :type fixnum)
  debug
  md-info)

(defun c-users (c)
  "Make it easier to change implementation"
  (fifo-data (c-users-store c)))

(defun user-ensure (used new-user)
  (unless (find new-user (c-users used))
    (fifo-add (c-users-store used) new-user)))

(defun user-drop (used user)
  (fifo-delete (c-users-store used) user))

(defmethod trcp ((c cell))
  nil #+(or) (and (typep (c-model c) 'index)
              (eql 'state (c-slot-name c))))

; --- ephemerality --------------------------------------------------
; 
; Not a type, but an option to the :cell parameter of defmodel
;
(defun c-ephemeral-p (c)
  (eql :ephemeral (md-slot-cell-type (type-of (c-model c)) (c-slot-name c))))

(defun c-ephemeral-reset (c)
  (when (c-ephemeral-p c) ;; so caller does not need to worry about this
    ;
    ; as of Cells3 we defer resetting ephemerals because everything
    ; else gets deferred and we cannot /really/ reset it until
    ; within finish-business we are sure all users have been recalculated
    ; and all outputs completed.
    ;
    (with-integrity (:ephemeral-reset c)
      (trc nil "!!!!!!!!!!!!!! c-ephemeral-reset resetting:" c)
      (md-slot-value-store (c-model c) (c-slot-name c) nil)
      (setf (c-value c) nil)))) ;; good q: what does (setf <ephem> 'x) return? historically nil, but...?

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

(defstruct (c-dependent
            (:include c-ruled)
            (:conc-name cd-))
  ;; chop (synapses nil :type list)
  (useds nil :type list)
  (usage (make-array 16 :element-type 'bit
                        :initial-element 0) :type simple-bit-vector))


(defstruct (c-stream
            (:include c-dependent)
            (:conc-name cs-))
  values)

(defstruct streamer from stepper donep to)

#+(or)
(defmacro c~~~ (&key (from 0)
                 stepper
                 (donep (c-lambda (> .cache (streamer-to slot-c))))
                 to)
   `(make-c-stream
     :rule (c-lambda (make-streamer
                      :from ,from
                      :stepper ,stepper
                      :to ,to :donep ,donep))))

;;;(defmethod md-slot-value-assume :around ((c c-stream) (s streamer))
;;;  (bif (to (streamer-to s))
;;;    (loop for slot-value = (streamer-from s)
;;;          then (bif (stepper (streamer-stepper s))
;;;                 (funcall stepper c)
;;;                 (incf slot-value))
;;;          until (bif (to (streamer-to s))
;;;                  (> slot-value to)
;;;                  (bwhen (donep-test (streamer-donep s))
;;;                    (funcall donep-test c)))
;;;          do (progn
;;;               (print `(assume doing ,slot-value))
;;;               (call-next-method c slot-value))))
;;;  (c-optimize-away?! c))

(defstruct (c-drifter
            (:include c-dependent)))

(defstruct (c-drifter-absolute
            (:include c-drifter)))

;_____________________ accessors __________________________________

(defmethod c-useds (other) (declare (ignore other)))
(defmethod c-useds ((c c-dependent)) (cd-useds c))

(defun c-validp (c)
  (eql (c-value-state c) :valid))

(defun c-unboundp (c)
  (eql :unbound (c-value-state c)))

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
