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

(eval-when (:compile-toplevel :execute :load-toplevel)
  (export '(model md-value family kids kid1 ^k1 kid2 ^k2 last-kid ^k-last perishable)))

(defmodel model ()
  ((.md-name :cell nil :initform nil :initarg :md-name :accessor md-name)
   (.fm-parent :cell nil :initform nil :initarg :fm-parent :accessor fm-parent)
   (.md-value :initform nil :accessor md-value :initarg :md-value)))


(defmethod fm-parent (other)
  (declare (ignore other))
  nil)

(defmethod print-object ((self model) s)
  (format s "~a" (or (md-name self) (type-of self))))

(define-symbol-macro .parent (fm-parent self))

(defmethod shared-initialize :around ((self model) slotnames &rest initargs &key fm-parent)
  (declare (ignorable initargs slotnames fm-parent))

  (call-next-method)

  (when (slot-boundp self '.md-name)
    (unless (md-name self)
      (setf (md-name self) (gentemp (string (c-class-name (class-of self)))))))
 
  (when (fm-parent self)
    (md-be-adopted self)))

(defmodel perishable ()
  ((expiration :initform nil :accessor expiration :initarg :expiration)))

(defobserver expiration ()
  (when new-value
    (not-to-be self)))

(defmodel family (model)
  ((.kid-slots :cell nil
         :initform nil
         :accessor kid-slots
         :initarg :kid-slots)
   (.kids :initform (c-in nil) ;; most useful
         :accessor kids
         :initarg :kids)
   ))

(defvar *parent*)

(defmacro the-kids (&rest kids)
  `(let ((*parent* self))
     (packed-flat! ,@kids)))

(defmacro s-sib-no () `(position self (kids .parent)))

(defmacro gpar ()
  `(fm-grandparent self))

(defmacro nearest (self-form type)
   (let ((self (gensym)))
   `(bwhen (,self ,self-form)
       (if (typep ,self ',type) ,self (upper ,self ,type)))))

(defun kid1 (self) (car (kids self)))
(defun kid2 (self) (cadr (kids self)))
(defmacro ^k1 () `(kid1 self))
(defmacro ^k2 () `(kid2 self))

(defun last-kid (self) (last1 (kids self)))
(defmacro ^k-last () `(last-kid self))

;; /// redundancy in following

(defmacro psib (&optional (self-form 'self))
  (let ((self (gensym)))
    `(bwhen (,self ,self-form)
        (find-prior ,self (kids (fm-parent ,self))))))

(defmacro nsib (&optional (self-form 'self))
  (let ((self (gensym)))
    `(bwhen (,self ,self-form)
        (cadr (member ,self (kids (fm-parent ,self)))))))

(defun prior-sib (self)
   (let ((kid (gensym)))
      `(let ((,kid ,self))
          (find-prior ,kid (kids (fm-parent ,kid))))))


(defun md-be-adopted (self &aux (fm-parent (fm-parent self)) (selftype (type-of self)))
    
  (c-assert self)
  (c-assert fm-parent)
  (c-assert (typep fm-parent 'family))
  

  (trc nil "md be adopted >" :kid self (adopt-ct self) :by fm-parent)
  
  (when (plusp (adopt-ct self))
    (c-break "2nd adopt ~a, by ~a" self fm-parent))

  (incf (adopt-ct self))

  (bwhen (kid-slots-fn (kid-slots (fm-parent self)))
    (dolist (ks-def (funcall kid-slots-fn self) self)
      (let ((slot-name (ks-name ks-def)))
        (trc nil "got ksdef " slot-name)
        (when (md-slot-cell-type selftype slot-name)
          (trc nil "got cell type " slot-name)
          (when (or (not (ks-if-missing ks-def))
                  (and (null (c-slot-value self slot-name))
                    (null (md-slot-cell self slot-name))))
            (trc nil "ks missing ok " slot-name)
            (multiple-value-bind (c-or-value suppressp)
                (funcall (ks-rule ks-def) self)
              (unless suppressp
                (trc nil "c-install " slot-name c-or-value)
                (c-install self slot-name c-or-value)))))))))

(defobserver .kids ((self family) new-kids old-kids)
  (declare (ignorable usage))
  (c-assert (listp new-kids))
  (c-assert (listp old-kids))
  (c-assert (not (member nil old-kids)))
  (c-assert (not (member nil new-kids)))
  (bwhen (sample (find-if-not 'fm-parent new-kids))
      (c-break "New as of Cells3: parent must be supplied to make-instance of ~a kid ~a"
        (type-of sample) sample))
  (trc nil ".kids output > entry" new-kids (mapcar 'fm-parent new-kids))

  (dolist (k (set-difference old-kids new-kids))
    (trc nil "kids change nailing lost kid" k)
    (not-to-be k)))

(defmethod kids ((other model-object))  nil)

(defmethod not-to-be :before ((fm family))
  (let ((sv-kids (slot-value fm '.kids)))
    (when (listp sv-kids)
      (dolist ( kid sv-kids)
        (not-to-be kid)))))

;------------------  kid slotting ----------------------------
;
(defstruct (kid-slotdef
           (:conc-name nil))
  ks-name
  ks-rule
  (ks-if-missing t))

(defmacro mk-kid-slot ((ks-name &key if-missing) ks-rule)
   `(make-kid-slotdef
     :ks-name ',ks-name
     :ks-rule (lambda (self)
                 (declare (ignorable self))
                 ,ks-rule)
     :ks-if-missing ,if-missing))

(defmacro def-kid-slots (&rest slot-defs)
  `(lambda (self)
     (declare (ignorable self))
     (list ,@slot-defs)))

(defmethod md-name (symbol)
     symbol)

(defmethod md-name ((nada null))
  (unless (c-stopped)
    (c-stop :md-name-on-null)
    (break "md-name called on nil")))

