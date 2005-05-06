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

;;;(eval-when (compile load)
;;;  (proclaim '(optimize (speed 1) (safety 1) (space 1) (debug 2))))

(eval-when (compile load)
  (proclaim '(optimize (speed 2) (safety 3) (space 1) (debug 3))))

(in-package :cells)

(define-constant *c-optimizep* t)
(defparameter *c-prop-depth* 0)
(defparameter *causation* nil)

(defparameter *data-pulse-id* 0)
(defparameter *data-pulses* nil)
(defparameter *unfinished-business* nil)
(defparameter *c-debug* nil)

(defun cell-reset ()
  (utils-kt-reset)
  (setf 
   *c-debug* nil
   *c-prop-depth* 0
   *data-pulse-id* 0
   *data-pulses* nil
   *unfinished-business* nil)
  (trc nil "------ cell reset ----------------------------"))

(defun c-stop (&optional why)
  (format t "~&C-STOP> stopping because ~a" why)
  (setf *stop* t))

(define-symbol-macro .stop
    (c-stop :user))

(defun c-stopped ()
  *stop*)

(defmacro c-assert (assertion &optional places fmt$ &rest fmt-args)
  (declare (ignore places))
  `(unless *stop*
     (unless ,assertion
       ,(if fmt$
            `(c-break ,fmt$ ,@fmt-args)
          `(c-break "failed assertion: ~a" ',assertion)))))

(defvar *c-calculators* nil)

(defmacro s-sib-no () `(position self (kids .parent)))

(defmacro gpar ()
  `(fm-grandparent self))

(defmacro nearest (self-form type)
   (let ((self (gensym)))
   `(bwhen (,self ,self-form)
       (if (typep ,self ',type) ,self (upper ,self ,type)))))

(defmacro def-c-trace (model-type &optional slot cell-type)
  `(defmethod trcp ((self ,(case cell-type
                             (:c? 'c-dependent)
                             (otherwise 'cell))))
     (and (typep (c-model self) ',model-type)
       ,(if slot
            `(eq (c-slot-name self) ',slot)
          `t))))

(defmacro without-c-dependency (&body body)
  `(let (*c-calculators*) ,@body))

(define-symbol-macro .cause
    (car *causation*))

(define-condition unbound-cell (unbound-slot) ())

(defgeneric c-output-slot-name (slotname self new old old-boundp)
  #-(or cormanlisp clisp)
  (:method-combination progn))

#-cells-testing
(defmethod c-output-slot-name #-(or cormanlisp clisp) progn
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
    (c-stop args)
    (format t "c-break > stopping > ~a" args)
    (apply #'error args)))




