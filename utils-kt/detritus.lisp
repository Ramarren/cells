;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils-kt; -*-
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

(in-package :utils-kt)

(defmacro wdbg (&body body)
  `(let ((*dbg* t))
     ,@body))

#+clisp
(defun slot-definition-name (slot)
  (clos::slotdef-name slot))

;;;(defmethod class-slot-named ((classname symbol) slotname)
;;;  (class-slot-named (find-class classname) slotname))
;;;
;;;(defmethod class-slot-named (class slotname)
;;;  (find slotname (class-slots class) :key #'slot-definition-name))

#+(and mcl (not openmcl-partial-mop))
(defun class-slots (c)
  (nconc (copy-list (class-class-slots c))
         (copy-list (class-instance-slots c))))


#-(or mcl)
(progn
  (defun true (it) (declare (ignore it)) t)
  (defun false (it) (declare (ignore it))))

(defun xor (c1 c2)
  (if c1 (not c2) c2))

(defun make-fifo-queue () (cons nil nil))
(defun fifo-add (q new)
  (if (car q)
      (let ((last (cdr q))
            (newlast (list new)))
        (rplacd last newlast)
        (rplacd q newlast))
    (let ((newlist (list new)))
      (rplaca q newlist)
      (rplacd q newlist))))
(defun fifo-queue (q) (car q))
(defun fifo-empty (q) (not (car q)))
(defun fifo-pop (q)
  (prog1
      (caar q)
    (rplaca q (cdar q))))

(defun mapfifo (fn q)
  (loop until (fifo-empty q)
      do (funcall fn (fifo-pop q))))

#+(or)
(let ((*print-circle* t))
  (let ((q (make-fifo-queue)))
    (loop for n below 3
      do (fifo-add q n))
    (fifo-queue q)
    (loop until (fifo-empty q)
          do (print (fifo-pop q)))))

(defmacro define-constant (name value &optional docstring)
  "Define a constant properly.  If NAME is unbound, DEFCONSTANT
it to VALUE.  If it is already bound, and it is EQUAL to VALUE,
reuse the SYMBOL-VALUE of NAME.  Otherwise, DEFCONSTANT it again,
resulting in implementation-specific behavior."
  `(defconstant ,name
     (if (not (boundp ',name))
	 ,value
	 (let ((value ,value))
	   (if (equal value (symbol-value ',name))
	       (symbol-value ',name)
	       value)))
     ,@(when docstring (list docstring))))
