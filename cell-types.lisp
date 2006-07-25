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
  (caller-store (make-fifo-queue) :type cons) ;; (C3) probably better to notify callers FIFO
  
  (state :nascent :type symbol) ;; :nascent, :awake, :optimized-away
  (value-state :unbound :type symbol) ;; {:unbound | :unevaluated | :valid}
  (pulse 0 :type fixnum)
  debug
  md-info)

(defun c-callers (c)
  "Make it easier to change implementation"
  (fifo-data (c-caller-store c)))

(defun caller-ensure (used new-caller)
  (unless (find new-caller (c-callers used))
    (fifo-add (c-caller-store used) new-caller)))

(defun caller-drop (used caller)
  (fifo-delete (c-caller-store used) caller))

(defmethod trcp ((c cell))
  #+not (and ;; (typep (c-model c) 'index)
   (find (c-slot-name c) '(celtk::state mathx::problem))))


; --- ephemerality --------------------------------------------------
; 
; Not a type, but an option to the :cell parameter of defmodel
;
(defun ephemeral-p (c)
  (eql :ephemeral (md-slot-cell-type (type-of (c-model c)) (c-slot-name c))))

(defun ephemeral-reset (c)
  (when (ephemeral-p c) ;; so caller does not need to worry about this
    ;
    ; as of Cells3 we defer resetting ephemerals because everything
    ; else gets deferred and we cannot /really/ reset it until
    ; within finish-business we are sure all callers have been recalculated
    ; and all outputs completed.
    ;
    ; ;; good q: what does (setf <ephem> 'x) return? historically nil, but...?
    ;
    (with-integrity (:ephemeral-reset c)
      (trc nil "!!!!!!!!!!!!!! ephemeral-reset resetting:" c)
      (md-slot-value-store (c-model c) (c-slot-name c) nil)
      (setf (c-value c) nil)
      #+notsureaboutthis
      (loop for caller in (c-callers c)
            do (calculate-and-link caller)))))

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
  (usage (blank-usage-mask)))

(defun blank-usage-mask ()
  (make-array 16 :element-type 'bit
    :initial-element 0))

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
  (unless (or *stop* *print-readably*)
    (format stream "[~a~a:" (if (c-inputp c) "i" "?")
      (cond
       ((null (c-model c)) #\0)
       ((eq :eternal-rest (md-state (c-model c))) #\_)
       ((not (c-currentp c)) #\#)
       (t #\space)))))


(defmethod print-object ((c cell) stream)
  (if (or *stop* *print-readably*)
      (call-next-method)
    (progn
      (c-print-value c stream)
      (format stream "=~d/~a/~a]"
        (c-pulse c)
        (symbol-name (or (c-slot-name c) :anoncell))
        (or (c-model c) :anonmd)))))


;__________________

(defmethod c-print-value ((c c-ruled) stream)
  (format stream "~a" (cond ((c-validp c) "<vld>")
                            ((c-unboundp c) "<unb>")
                            ((not (c-currentp c)) "dirty")
                            (t "<err>"))))

(defmethod c-print-value (c stream)
  (declare (ignore c stream)))



