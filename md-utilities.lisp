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
  (:method ((self model-object))
    (md-quiesce self))

  (:method :around ((self model-object))
    (declare (ignorable self))
    (let ((*not-to-be* t))
      (trc nil #+not (typep self '(or mathx::problem mathx::prb-solvers mathx::prb-solver))
        "not.to-be nailing" self)
      (unless (eq (md-state self) :eternal-rest)
        (call-next-method)
        
        (setf (fm-parent self) nil
          (md-state self) :eternal-rest)

        (md-map-cells self nil
          (lambda (c)
            (c-assert (eq :quiesced (c-state c))))) ;; fails if user obstructs not.to-be with primary method (use :before etc)

        (trc nil "not.to-be cleared 2 fm-parent, eternal-rest" self)))))

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

