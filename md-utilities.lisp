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

(defun md-awake (self) (eql :awake (md-state self)))

(defun fm-grandparent (md)
  (fm-parent (fm-parent md)))


(defmethod md-release (other)
  (declare (ignorable other)))

(export! mdead)
;___________________ birth / death__________________________________
  
(defgeneric mdead (self)
  (:method ((self model-object))
    (unless *not-to-be*
      (eq :eternal-rest (md-state self))))

  (:method (self)
    (declare (ignore self))
    nil))

(defgeneric not-to-be (self)
  (:method ((self list))
    (dolist (s self)
      (not-to-be s)))
  (:method ((self array))
    (loop for s across self
          do (not-to-be s)))
  (:method ((self hash-table))
    (maphash (lambda (k v)
               (declare (ignorable k))
               (not-to-be v)) self))

  (:method ((self model-object))
    (md-quiesce self))
  
  (:method :before ((self model-object))
    (loop for (slot-name . owning?) in (get (type-of self) :ownings)
        when owning?
        do (not-to-be (slot-value self slot-name))))

  (:method :around ((self model-object))
    (declare (ignorable self))
    (let ((*not-to-be* t)
          (dbg nil #+not (or (eq (md-name self) :eclm-owner)
                 (typep self '(or mathx::eclm-2008 clo:ix-form mathx::a1-panel mathx::edit-caret ctk:window)))))
      
      (flet ((gok ()
               (unless (eq (md-state self) :eternal-rest)
                 (call-next-method)
                 
                 (setf (fm-parent self) nil
                   (md-state self) :eternal-rest)
                 
                 (md-map-cells self nil
                   (lambda (c)
                     (c-assert (eq :quiesced (c-state c)) ()
                       "Cell ~a of dead model ~a not quiesced. Was not-to-be shadowed by
 a primary method? Use :before instead."))) ;; fails if user obstructs not.to-be with primary method (use :before etc)
                 
                 )))
        (if (not dbg)
            (gok)
          (wtrc (0 100 "not.to-be nailing" self (when (typep self 'family)
                                                  (mapcar 'type-of (slot-value self '.kids))))
            (gok)
            (when dbg (trc "finished nailing" self))))))))
  
(defun md-quiesce (self)
  (trc nil "md-quiesce nailing cells" self (type-of self))
  (md-map-cells self nil (lambda (c)
                           (trc nil "quiescing" c)
                           (c-assert (not (find c *call-stack*)))
                           (c-quiesce c))))

(defun c-quiesce (c)
  (typecase c
    (cell 
     (trc nil "c-quiesce unlinking" c)
     (c-unlink-from-used c)
     (dolist (caller (c-callers c))
       (setf (c-value-state caller) :uncurrent)
       (trc nil "c-quiesce unlinking caller and making uncurrent" :q c :caller caller)
       (c-unlink-caller c caller))
     (setf (c-state c) :quiesced) ;; 20061024 for debugging for now, might break some code tho
     )))

(defparameter *to-be-dbg* nil)

(defmacro make-kid (class &rest initargs)
  `(make-instance ,class
     ,@initargs
     :fm-parent (progn (assert self) self)))

