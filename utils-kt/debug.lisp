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


(defvar *count* nil)
(defvar *counting* nil)
(defvar *dbg*)
(defvar *stop* nil)

(defun utils-kt-reset ()
  (clock-off :ukt-reset)
  (setf *count* nil
    *stop* nil
    *dbg* nil)
  
 (print "----------UTILSRESET----------------------------------"))

;------------- counting ---------------------------

(defmacro with-counts ((onp &rest msg) &body body)
  `(if ,onp
       (let ((*counting* (cons t *counting*)))
         (prog2
           (count-clear nil ,@msg)
             (progn ,@body)
           (show-count t ,@msg)))
     (progn ,@body)))

(defun count-of (key)
  (cdr (assoc key *count* :key 'car)))
  
(defun count-clear (announce &rest msg)
  (declare (ignorable msg))
  (when announce (format t "~&count-clear > ~a" msg))
  (setf *count* nil))

(defmacro count-it (&rest keys)
  (declare (ignorable keys))
  #+nahhh
  `(progn)
  `(when (car *counting*)
     (call-count-it ,@keys)))

(export! count-it!)
(defmacro count-it! (&rest keys)
  (declare (ignorable keys))
  #+(and its-alive! (not debugging-alive!))
  `(progn)
  #-(and its-alive! (not debugging-alive!))
  `(when (car *counting*)
     (call-count-it ,@keys)))

(defun call-count-it (&rest keys)
    (declare (ignorable keys))
  #+nahh (when (find (car keys) '(:trcfailed :TGTNILEVAL))
           (break "clean up time ~a" keys))
  (let ((entry (assoc keys *count* :test #'equal)))
      (if entry
          (setf (cdr entry) (1+ (cdr entry)))
        (push (cons keys 1) *count*))))

(defun show-count (clearp &rest msg &aux announced)
  
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
           and do (unless announced
                    (setf announced t)
                    (format t "~&Counts after: clearp ~a, length ~d: ~s" clearp (length *count*) msg))
           (format t "~&~4d ... ~2d ... ~(~{~a ~}~)" running occs (car entry))))
  (when clearp (count-clear announced "show-count" )))
               
;-------------------- timex ---------------------------------

(export! timex)

(defmacro timex ((onp &rest trcargs) &body body)
  `(if ,onp
       (prog2
           (format t "~&Starting timing run of ~{ ~a~}" (list ,@trcargs))
           (time (progn ,@body))
         (format t "~&Above timing was of ~{ ~a~}" (list ,@trcargs)))
     (progn ,@body)))

#+save
(defun dbg-time-report (cpu-gc-user cpu-gc-sys cpu-tot-user cpu-tot-sys real-time conses other-bytes static-bytes)
  (format t "~&cpu-gc-user ~a" cpu-gc-user)
  (format t "~&cpu-gc-sys ~a" cpu-gc-sys)
  (format t "~&cpu-tot-user ~a" cpu-tot-user)
  (format t "~&cpu-tot-sys ~a" cpu-tot-sys)
  (format t "~&<non-gc user cpu> ~a" (- cpu-tot-user cpu-gc-user))
  (format t "~&<non-gc sys cpu> ~a" (- cpu-tot-sys cpu-gc-sys))
  (format t "~&conses ~a" conses)
  (format t "~&other-bytes ~a" other-bytes)
  (format t "~&static-bytes ~a" static-bytes)
  (excl::time-report cpu-gc-user cpu-gc-sys cpu-tot-user cpu-tot-sys real-time conses other-bytes static-bytes))

;---------------- Metrics -------------------

(defmacro with-metrics ((countp timep &rest trcargs) form-measured &body postlude)
  `(with-counts (,countp ,@trcargs)
     (timex (,timep ,@trcargs)
       ,form-measured)
     ,@postlude))

(defvar *clock*)

(export! clock clock-0 clock-off)

(defun clock-off (key)
  (when (boundp '*clock*)
    (print (list :clock-off key))
    (makunbound '*clock*)))

(defun clock-0 (key &aux (now (get-internal-real-time)))
  (setf *clock* (cons now now))
  (print (list :clock-initialized-by key)))

(defun clock (&rest keys &aux (now (get-internal-real-time)))
  (when (boundp '*clock*)
    (print (list* :clock (- now (cdr *clock*)) :tot (- now (car *clock*)) :at keys))
    (setf (cdr *clock*) now)))

