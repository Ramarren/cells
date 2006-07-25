;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils-kt; -*-
;;;
#|

    Utils-kt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#


(in-package :utils-kt)

(defparameter *trcdepth* 0)
(defvar *count* nil)
(defvar *counting* nil)
(defvar *dbg*)
(defvar *stop* nil)

(defun utils-kt-reset ()
  (setf *count* nil
    *stop* nil
    *dbg* nil
    *trcdepth* 0)
  (print "----------UTILSRESET----------------------------------"))

;----------- trc -------------------------------------------

(defun trcdepth-reset ()
  (setf *trcdepth* 0))

(defmacro trc (tgt-form &rest os
                &aux (wrapper (if (macro-function 'without-c-dependency)
                                  'without-c-dependency 'progn)))
  (if (eql tgt-form 'nil)
      '(progn)
    (if (stringp tgt-form)
        `(,wrapper
          (call-trc t ,tgt-form ,@os))
      (let ((tgt (gensym)))
        `(,wrapper
          (bif (,tgt ,tgt-form)
            (if (trcp ,tgt)
                (progn
                  (assert (stringp ,(car os)))
                  (call-trc t ,@os)) ;;,(car os) ,tgt ,@(cdr os)))
              (progn
                ;; (break "trcfailed")
                (count-it :trcfailed)))
            (count-it :tgtnileval)))))))

(defun call-trc (stream s &rest os)
  (if #+cormanlisp nil #-cormanlisp (and (boundp '*trcdepth*)
                                      *trcdepth*)
    (format stream "~&~v,,,'.<~d~>> " (mod *trcdepth* 100) *trcdepth*)
    (format stream "~&"))
  
  (format stream "~a" s)
  (let (pkwp)
    (dolist (o os)
      (format stream (if pkwp " ~s" " | ~s") o)
      (setf pkwp (keywordp o))))
  (force-output stream)
  (values))
  
(defun call-trc-to-string (fmt$ &rest fmt-args)
    (let ((o$ (make-array '(0) :element-type 'base-char
                :fill-pointer 0 :adjustable t)))
      (with-output-to-string (os-stream o$)
        (apply 'call-trc os-stream fmt$ fmt-args))
      o$))

#+findtrcevalnils
(defmethod trcp :around (other)
  (unless (call-next-method other)(break)))

(defmethod trcp (other)
  (eq other t))
  
(defmethod trcp (($ string))
  t)
  
(defun trcdepth-incf ()
  (incf *trcdepth*))
  
(defun trcdepth-decf ()
  (format t "decrementing trc depth ~d" *trcdepth*)
  (decf *trcdepth*))
  
(export! wtrc)

(defmacro wtrc ((&optional (min 1) (max 50) &rest banner) &body body )
  `(let ((*trcdepth* (if *trcdepth*
                         (1+ *trcdepth*)
                       0)))
     ,(when banner `(when (>= *trcdepth* ,min)
                      (if (< *trcdepth* ,max)
                          (trc ,@banner)
                        (progn
                          (break "excess trace notttt!!! ~d" *trcdepth*) ;; ,@banner)
                          nil))))
     (when (< *trcdepth* ,max)
       ,@body)))

(defmacro wnotrc ((&optional (min 1) (max 50) &rest banner) &body body )
  (declare (ignore min max banner))
  `(progn ,@body))
  
;------ eko --------------------------------------


(defmacro eko ((&rest trcargs) &rest body)
  (let ((result (gensym)))
     `(let ((,result ,@body))
         (trc ,(car trcargs) :=> ,result ,@(cdr trcargs))
         ,result)))

(defmacro eko-if ((test &rest trcargs) &rest body)
  (let ((result (gensym)))
     `(let ((,result ,@body))
         (when ,test
           (trc ,(car trcargs) :=> ,result ,@(cdr trcargs)))
         ,result)))

(defmacro ek (label &rest body)
  (let ((result (gensym)))
     `(let ((,result (,@body)))
         (when ,label
           (trc ,label ,result))
         ,result)))

;------------- counting ---------------------------


(defmacro with-counts ((onp &rest msg) &body body)
  `(if ,onp
       (let ((*counting* (cons t *counting*)))
         (prog2
           (count-clear ,@msg)
             (progn ,@body)
           (show-count t ,@msg)))
     (progn ,@body)))

(defun count-of (key)
  (cdr (assoc key *count* :key 'car)))
  
(defun count-clear (&rest msg)
  (declare (ignorable msg))
  (format t "~&count-clear > ~a" msg)
  (setf *count* nil))

(defmacro count-it (&rest keys)
  (declare (ignorable keys))
  #+(or) `(progn)
  `(when *counting*
     (call-count-it ,@keys)))

(defun call-count-it (&rest keys)
    (declare (ignorable keys))
  ;;; (when (eql :TGTNILEVAL (car keys))(break))
  (let ((entry (assoc keys *count* :test #'equal)))
      (if entry
          (setf (cdr entry) (1+ (cdr entry)))
        (push (cons keys 1) *count*))))

(defun show-count (clearp &rest msg)
  (format t "~&Counts after: clearp ~a, length ~d: ~s" clearp (length *count*) msg)
  (let ((res (sort (copy-list *count*) (lambda (v1 v2)
                                           (let ((v1$ (symbol-name (caar v1)))
                                                 (v2$ (symbol-name (caar v2))))
                                             (if (string= v1$ v2$)
                                                 (< (cdr v1) (cdr v2))
                                               (string< v1$ v2$))))))
        )
     (loop for entry in res
         for occs = (cdr entry)
         when (plusp occs)
           sum occs into running
           and do (format t "~&~4d ... ~2d ... ~s" running occs (car entry))))
  (when clearp (count-clear "show-count")))
  

;-------------------- timex ---------------------------------

(eval-when (compile eval load)
  (export '(timex)))

(defmacro timex ((onp &rest trcargs) &body body)
  `(if ,onp
       (prog1
           (time (progn ,@body))
         (trc "timing was of" ,@trcargs))
     (progn ,@body)))

#+save
(defun dbg-time-report (cpu-gc-user cpu-gc-sys cpu-tot-user cpu-tot-sys real-time conses other-bytes static-bytes)
  (trc "cpu-gc-user" cpu-gc-user)
  (trc "cpu-gc-sys" cpu-gc-sys)
  (trc "cpu-tot-user" cpu-tot-user)
  (trc "cpu-tot-sys" cpu-tot-sys)
  (trc "<non-gc user cpu>" (- cpu-tot-user cpu-gc-user))
  (trc "<non-gc sys cpu>" (- cpu-tot-sys cpu-gc-sys))
  (trc "conses" conses)
  (trc "other-bytes" other-bytes)
  (trc "static-bytes" static-bytes)
  (excl::time-report cpu-gc-user cpu-gc-sys cpu-tot-user cpu-tot-sys real-time conses other-bytes static-bytes))

;---------------- Metrics -------------------

(defmacro with-metrics ((countp timep &rest trcargs) form-measured &body postlude)
  `(with-counts (,countp ,@trcargs)
     (timex (,timep ,@trcargs)
       ,form-measured)
     ,@postlude))

