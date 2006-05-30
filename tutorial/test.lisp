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

(eval-when (compile load)
  (proclaim '(optimize (speed 2) (safety 3) (space 1) (debug 3))))

(defpackage #:tu-cells
  (:use :cl :utils-kt)
  (:export #:clear-computed #:verify-computed #:verify-not-computed #:compute-log))

(in-package :tu-cells)

(defmacro ct-assert (form &rest stuff)
  `(progn
     (print `(attempting ,',form))
    (assert ,form () "Error with ~a >> ~a" ',form (list ,@stuff))))

(defvar *computed*)
(defun clear-computed ()
  (setf *computed* nil))

(defun compute-log (&rest keys)
  (loop for k in keys
        do (pushnew k *computed*)))

(defun verify-computed (&rest keys)
  (loop for k in keys
        do (assert (find k *computed*)() "Unable verify ~a computed: ~a" k *computed*)))

(defun verify-not-computed (&rest keys)
  (loop for k in keys
        do (assert (not (find k *computed*)) () "Unable verify ~a NOT computed: ~a" k *computed*)
        finally (return t)))