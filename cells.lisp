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

(eval-when (compile load)
  (proclaim '(optimize (speed 2) (safety 3) (space 1) (debug 3))))

(in-package :cells)

(defparameter *c-prop-depth* 0)
(defparameter *causation* nil)

(defparameter *data-pulse-id* 0)
(defparameter *data-pulses* nil)

(defparameter *c-debug* nil)
(defparameter *defer-changes* nil)
(defparameter *within-integrity* nil)
(defparameter *client-queue-handler* nil)
(defparameter *unfinished-business* nil)
(defun cells-reset (&optional client-queue-handler)
  (utils-kt-reset)
  (setf 
   *c-prop-depth* 0
   *data-pulse-id* 0
   *data-pulses* nil
   *defer-changes* nil ;; should not be necessary, but cannot be wrong
   *client-queue-handler* client-queue-handler
   *within-integrity* nil
   *unfinished-business* nil)
  (trc nil "------ cell reset ----------------------------"))

(defun c-stop (&optional why)
  (setf *stop* t)
  (format t "~&C-STOP> stopping because ~a" why)  )

(define-symbol-macro .stop
    (c-stop :user))

(defun c-stopped ()
  *stop*)

(defmacro c-assert (assertion &optional places fmt$ &rest fmt-args)
  (declare (ignorable assertion places fmt$ fmt-args))
   #+(or)`(progn) 
  `(unless *stop*
     (unless ,assertion
       ,(if fmt$
            `(c-break ,fmt$ ,@fmt-args)
          `(c-break "failed assertion: ~a" ',assertion)))))

(defvar *call-stack* nil)

(defmacro def-c-trace (model-type &optional slot cell-type)
  `(defmethod trcp ((self ,(case cell-type
                             (:c? 'c-dependent)
                             (otherwise 'cell))))
     (and (typep (c-model self) ',model-type)
       ,(if slot
            `(eq (c-slot-name self) ',slot)
          `t))))

(defmacro without-c-dependency (&body body)
  `(let (*call-stack*) ,@body))

(define-symbol-macro .cause
    (car *causation*))

(define-condition unbound-cell (unbound-slot)
  ((cell :initarg :cell :reader cell :initform nil)))

(defgeneric slot-value-observe (slotname self new old old-boundp)
  #-(or cormanlisp)
  (:method-combination progn))

#-cells-testing
(defmethod slot-value-observe #-(or cormanlisp) progn
  (slot-name self new old old-boundp)
  (declare (ignorable slot-name self new old old-boundp)))


; -------- cell conditions (not much used) ---------------------------------------------

(define-condition xcell () ;; new 2k0227
  ((cell :initarg :cell :reader cell :initform nil)
   (app-func :initarg :app-func :reader app-func :initform 'bad-cell)
   (error-text :initarg :error-text :reader error-text :initform "<???>")
   (other-data :initarg :other-data :reader other-data :initform "<nootherdata>"))
  (:report (lambda (c s)
             (format s "~& trouble with cell ~a in function ~s,~s: ~s"
               (cell c) (app-func c) (error-text c) (other-data c)))))

(define-condition c-enabling ()
   ((name :initarg :name :reader name)
    (model :initarg :model :reader model)
    (cell :initarg :cell :reader cell))
   (:report (lambda (condition stream)
                 (format stream "~&unhandled <c-enabling>: ~s" condition)
                 (break "~&i say, unhandled <c-enabling>: ~s" condition))))

(define-condition c-fatal (xcell)
   ((name :initarg :name :reader name)
    (model :initarg :model :reader model)
    (cell :initarg :cell :reader cell))
   (:report (lambda (condition stream)
              (format stream "~&fatal cell programming error: ~s" condition)
              (format stream "~&  : ~s" (name condition))
              (format stream "~&  : ~s" (model condition))
              (format stream "~&  : ~s" (cell condition)))))

(define-condition c-unadopted (c-fatal)
   ()
   (:report
    (lambda (condition stream)
      (format stream "~&unadopted cell >: ~s" (cell condition))
      (format stream "~& >: often you mis-edit (c? (c? ...)) nesting is error"))))


(defun c-break (&rest args)
  (unless *stop*
    (LET ((*print-level* 3)
          (*print-circle* t)
          )
      (c-stop args)
      (format t "c-break > stopping > ~a" args)
      (apply 'break args))))




