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


;----------------- model-object ----------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(md-name fm-parent .parent)))

(defclass model-object ()
  ((.md-state :initform nil :accessor md-state) ; [nil | :nascent | :alive | :doomed]
   (.cells :initform nil :accessor cells)
   (.cells-flushed :initform nil :accessor cells-flushed
                   :documentation "cells supplied but un-whenned or optimized-away")
   (adopt-ct :initform 0 :accessor adopt-ct)))

(defmethod md-slot-cell (self slot-name)
  (cdr (assoc slot-name (cells self))))

(defun md-slot-cell-type (class-name slot-name)
  (bif (entry (assoc slot-name (get class-name :cell-types)))
    (cdr entry)
    (dolist (super (class-precedence-list (find-class class-name)))
      (bwhen (entry (assoc slot-name (get (c-class-name super) :cell-types)))
        (return (setf (md-slot-cell-type class-name slot-name) (cdr entry)))))))       

(defun (setf md-slot-cell-type) (new-type class-name slot-name)
  (let ((entry (assoc slot-name (get class-name :cell-types))))
    (if entry
        (setf (cdr entry) new-type)
      (push (cons slot-name new-type) (get class-name :cell-types)))))

(defmethod md-slot-value-store ((self model-object) slot-name new-value)
  (setf (slot-value self slot-name) new-value))

(defun md-slot-cell-flushed (self slot-name)
  (cdr (assoc slot-name (cells-flushed self))))

;----------------- navigation: slot <> initarg <> esd <> cell -----------------

#+cmu
(defmethod c-class-name ((class pcl::standard-class))
  (pcl::class-name class))

(defmethod c-class-name (other) (declare (ignore other)) nil)

(defmethod c-class-name ((class standard-class))
  (class-name class))

(defmethod cell-when (other) (declare (ignorable other)) nil)

(defun (setf md-slot-cell) (new-cell self slot-name)
  (bif (entry (assoc slot-name (cells self)))
    (let ((old (cdr entry))) ;; s/b being supplanted by kid-slotter
      (c-assert (null (c-users old)))
      (c-assert (null (cd-useds old)))
      (trc nil "replacing in model .cells" old new-cell self)
      (rplacd entry new-cell))
    (progn
      (trc nil "adding to model .cells" new-cell self)
      (push (cons slot-name new-cell)
        (cells self)))))

(defun md-map-cells (self type celldo)
  (map type (lambda (cell-entry)
                (bwhen (cell (cdr cell-entry))
                       (unless (listp cell)
                         (funcall celldo cell))))
        (cells self)))

(defun c-install (self sn c &aux (c-isa-cell (typep c 'cell)))
  (when c-isa-cell
    (count-it :c-install)
    (setf
     (c-model c) self
     (c-slot-name c) sn
     (md-slot-cell self sn) c))

  (if c-isa-cell
      (if (c-unboundp c)
          (bd-slot-makunbound self sn)
        (setf (slot-value self sn)
          (if (c-inputp c)
                  (c-value c)
                nil)))
    (setf (slot-value self sn) c)))

;------------------ md obj initialization ------------------

(defmethod shared-initialize :after ((self model-object) slotnames
                                      &rest initargs &key fm-parent)
  (declare (ignorable initargs slotnames fm-parent))
  (dolist (esd (class-slots (class-of self)))
    (let* ((sn (slot-definition-name esd))
           (sv (when (slot-boundp self sn)
                 (slot-value self sn))))
      (when (typep sv 'cell)
        (if (md-slot-cell-type (type-of self) sn)
            (c-install self sn sv)
          (when *c-debug*
            (trc "warning: cell ~a offered for non-cellular model/slot ~a/~a" sv self sn))))))

  (md-initialize self))

(defmethod md-initialize (self)
  (setf (md-state self) :nascent))

;--------- awaken only when ready (in family, for models) --------

(defmethod md-awaken ((self model-object))
  (trc nil "md-awaken entry" self (md-state self))
  (c-assert (eql :nascent (md-state self)))
  (count-it :md-awaken)
  ;;(count-it 'mdawaken (type-of self))
  (setf (md-state self) :awakening)
  (dolist (esd (class-slots (class-of self)))
    (when (md-slot-cell-type (type-of self) (slot-definition-name esd))
      (let* ((slot-name (slot-definition-name esd))
             (c (md-slot-cell self slot-name)))
        (when *c-debug*
          (bwhen (sv (and (slot-boundp self slot-name)
                       (slot-value self slot-name)))
            (when (typep sv 'cell)
              (c-break "md-awaken ~a found cell ~a in slot ~a" self sv esd))))
          
        (if c
            (cond
             ((find (c-lazy c) '(:until-asked :always t))
              (trc nil "md-awaken deferring c-awaken since lazy" 
                self esd))
             ((eq :nascent (c-state c)) (c-awaken c)))
            
          (progn
            (when (eql '.kids slot-name)
              (bwhen (sv (slot-value self '.kids))
                (md-kids-change self sv nil :md-awaken-slot)))
            (c-output-slot nil slot-name self (bd-slot-value self slot-name) nil nil))))))
  
  (setf (md-state self) :awake)
  self)
  
