;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
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

(eval-when (compile :execute load)
  (proclaim '(optimize (speed 2) (safety 3) (space 1) (debug 3)))
  (defmacro cv-assert (form &optional places (datum "~&~a~&...failed") &rest args)
  `(progn
     (assert  ,form ,places ,datum ,@(or args (list `',form)))
     (format t "~&ok: ~a~&" ',form))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :cells-testing *features*))

(defvar *failed-tests* ())

(defmacro with-testing (&body tests)
  `(let ((*failed-tests* ()))
     ,@(loop for form in tests
	     for (test-name) = form
	     collect `(let ((failed t))
			(with-simple-restart
			    (accept-failed "Mark test ~A as failed." ',test-name)
			  ,form
			  (setf failed nil))
			(when failed
			  (push ',test-name *failed-tests*))))
     (when *failed-tests*
       (setf *failed-tests* (nreverse *failed-tests*))
       (format t "~&~D out of ~D tests failed: ~{~&    ~A~%~}"
	       (length *failed-tests*) ',(length tests) *failed-tests*))))

(defun cv-test ()
  (let ((*c-debug* t))
    (with-testing
        (cell-reset)
      (test-cells)
      (cell-reset)
      (hello-world) ;; non-assertive
    
      (cv-test-engine)
      (cv-test-person)
      (df-test)
      (cv-test-family)
      (cv-family-values)
      (cv-kid-slotting)
      (boiler-1)
      (boiler-2)
      (boiler-3) ;; non-assertive
      (boiler-4) ;; non-assertive
      (cv-laziness)
      (cv-output-setf)
      (cv-test-lazy))))


#+test
(progn
  (let ((*c-debug* t))
    (cell-reset)
    ;(hello-world) ;; non-assertive
    (cv-test-engine)
    ;;;    (cv-test-person)
    ;;;    ;; should fail: (df-test nil)
    ;;;    (df-test t)
    ;;;    (cv-test-family)
    ;;;    (cv-family-values)
    ;;;    (cv-kid-slotting)
    ;;;    (boiler-1)
    ;;;    (boiler-2)
    ;;;    (boiler-3) ;; non-assertive
    ;;;    (boiler-4) ;; non-assertive
    ))

(defun dft ()
  (let ();(*c-debug* t))
    (cell-reset)
    (df-test)
    ))

(defun output-clear (slot-name)
  (setf (getf (symbol-plist slot-name) 'outputted) nil)
  (setf (getf (symbol-plist slot-name) 'output-new-value) :unbound)
  (setf (getf (symbol-plist slot-name) 'output-old-value) :unbound)
  (setf (getf (symbol-plist slot-name) 'output-old-boundp) nil))

(defun outputted (slot-name)
  (getf (symbol-plist slot-name) 'outputted))

(defun output-new (slot-name)
  (bwhen (nv (getf (symbol-plist slot-name) 'output-new-value))
    (unless (eql nv :unbound) nv)))


(defun output-old (slot-name)
  (bwhen (nv (getf (symbol-plist slot-name) 'output-old-value))
    (unless (eql nv :unbound) nv)))

(defun output-old-boundp (slot-name)
  (getf (symbol-plist slot-name) 'output-old-boundp))


;; ---------------------------------------------------------
;; the redefinition warning on this next method is OK, just don't
;; load this unless running the regression test on cells
;;
#+cells-testing
(defmethod c-output-slot-name
    #-(or cormanlisp clisp) progn
  #+(or cormanlisp clisp) :before
  (slot-name self new old old-boundp)
  (declare (ignorable slot-name self new old old-boundp))
  #-runtime-system
  (progn
    (trc nil "output registering" slot-name new old old-boundp)
    (setf (getf (symbol-plist slot-name) 'outputted) t)
    (setf (getf (symbol-plist slot-name) 'output-new-value) new)
    (setf (getf (symbol-plist slot-name) 'output-old-value) old)
    (setf (getf (symbol-plist slot-name) 'output-old-boundp) old-boundp)))

