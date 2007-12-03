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

(in-package :cells)

(eval-when (:compile-toplevel :execute :load-toplevel)
  (export '(model value family dbg
             kids kid1 ^k1 kid2 ^k2 last-kid ^k-last perishable)))

(defmodel model ()
  ((.md-name :cell nil :initform nil :initarg :md-name :accessor md-name)
   (.fm-parent :cell nil :initform nil :initarg :fm-parent :accessor fm-parent)
   (.value :initform nil :accessor value :initarg :value)
   (zdbg :initform nil :accessor dbg :initarg :dbg))
  )


(defmethod print-cell-object ((md model))
  (or (md-name md) :md?))

(defmethod fm-parent (other)
  (declare (ignore other))
  nil)

(defmethod (setf fm-parent) (new-value other)
  (declare (ignore other))
  new-value)

(defmethod print-object ((self model) s)
  #+shhh (format s "~a" (type-of self))
  (format s "~a~a" (if (mdead self) "DEAD!" "")
    (or (md-name self) (type-of self))))

(define-symbol-macro .parent (fm-parent self))

(defmethod md-name (other)
  (trc "yep other md-name" other (type-of other))
  other)

(defmethod md-name ((nada null))
  (unless (c-stopped)
    (c-stop :md-name-on-null)
    (break "md-name called on nil")))

(defmethod md-name ((sym symbol)) sym)

(defmethod shared-initialize :around ((self model) slotnames &rest initargs &key fm-parent)
  (declare (ignorable initargs slotnames fm-parent))

  (call-next-method)

  (when (slot-boundp self '.md-name)
    (unless (md-name self)
      (setf (md-name self) (gentemp (string (c-class-name (class-of self)))))))
 
  (when (and (slot-boundp self '.fm-parent)
          (fm-parent self)
          (zerop (adopt-ct self)))
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
     :owning t
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

(export! first-born-p)
(defun first-born-p (self)
  (eq self (kid1 .parent)))

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
  (trc nil "getting adopted" self :by fm-parent)
  (bwhen (kid-slots-fn (kid-slots (fm-parent self)))
    (dolist (ks-def (funcall kid-slots-fn self) self)
      (let ((slot-name (ks-name ks-def)))
        (trc nil "got ksdef " slot-name (ks-if-missing ks-def))
        (when (md-slot-cell-type selftype slot-name)
          (trc nil "got cell type " slot-name )
          (when (or (not (ks-if-missing ks-def))
                  (and (null (c-slot-value self slot-name))
                    (null (md-slot-cell self slot-name))))
            (trc nil "ks missing ok " slot-name)
            (multiple-value-bind (c-or-value suppressp)
                (funcall (ks-rule ks-def) self)
              (unless suppressp
                (trc nil "md-install-cell " slot-name c-or-value)
                (md-install-cell self slot-name c-or-value)))))))))

(defobserver .kids ((self family) new-kids old-kids)
  (c-assert (listp new-kids) () "New kids value for ~a not listp: ~a ~a" self (type-of new-kids) new-kids)
  (c-assert (listp old-kids))
  (c-assert (not (member nil old-kids)))
  (c-assert (not (member nil new-kids)))
  (bwhen (sample (find-if-not 'fm-parent new-kids))
      (c-break "New as of Cells3: parent must be supplied to make-instance of ~a kid ~a"
        (type-of sample) sample))
  (trc nil ".kids output > entry" new-kids (mapcar 'fm-parent new-kids)))

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



