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

(defun c-setting-debug (self slot-name c new-value)
  (declare (ignorable new-value))
  (if (null c)
      (progn
        (format t "c-setting-debug > constant  ~a in ~a may not be altered..init to (c-in nil)"
              slot-name self)
        
        (c-break "setting-const-cell")
        (error "setting-const-cell"))
    (let ((self (c-model c))
          (slot-name (c-slot-name c)))
      ;(trc "c-setting-debug sees" c newvalue self slot-name)
      (when (and c (not (and slot-name self)))
        ;; cv-test handles errors, so don't set *stop* (c-stop)
        (c-break "unadopted ~a for self ~a spec ~a" c self slot-name)
        (error 'c-unadopted :cell c))
      (typecase c
        (c-dependent
         ;(trc "setting c-dependent" c newvalue)
         (format t "c-setting-debug > ruled  ~a in ~a may not be setf'ed"
           (c-slot-name c) self)
         
         (c-break "setting-ruled-cell")
         (error "setting-ruled-cell"))
        ))))

(defun c-absorb-value (c value)
  (typecase c
    (c-drifter-absolute (c-value-incf c value 0)) ;; strange but true
    (c-drifter (c-value-incf c (c-value c) value))
    (t value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(c-value-incf)))

(defmethod c-value-incf (c (envaluer c-envaluer) delta)
  (c-assert (c-model c))
  (c-value-incf c (funcall (envalue-rule envaluer) c)
                 delta))

(defmethod c-value-incf (c (base number) delta)
  (declare (ignore c))
  (if delta
    (+ base delta)
    base))


;----------------------------------------------------------------------

(defun bd-slot-value (self slot-name)
  (slot-value self slot-name))

(defun (setf bd-slot-value) (new-value self slot-name)
  (setf (slot-value self slot-name) new-value))

(defun bd-bound-slot-value (self slot-name caller-id)
  (declare (ignorable caller-id))
  (when (bd-slot-boundp self slot-name)
    (bd-slot-value self slot-name)))

(defun bd-slot-boundp (self slot-name)
  (slot-boundp self slot-name))

(defun bd-slot-makunbound (self slot-name)
  (slot-makunbound self slot-name))

#| sample incf
(defmethod c-value-incf ((base fpoint) delta)
  (declare (ignore model))
  (if delta
    (fp-add base delta)
    base))
|#
