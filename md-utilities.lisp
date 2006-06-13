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

;___________________ birth / death__________________________________
  
(defmethod not-to-be :around (self)
  (trc nil "not-to-be clearing 1 fm-parent, eternal-rest" self)
  (c-assert (not (eq (md-state self) :eternal-rest)))

  (call-next-method)
  
  (setf (fm-parent self) nil
    (md-state self) :eternal-rest)
  (trc nil "not-to-be cleared 2 fm-parent, eternal-rest" self))

(defmethod not-to-be ((self model-object))
  (trc nil "not to be!!!" self)
  (md-quiesce self))

(defun md-quiesce (self)
  (trc nil "md-quiesce doing" self (type-of self))
  (md-map-cells self nil (lambda (c)
                           (trc nil "quiescing" c)
                           (c-assert (not (find c *c-calculators*)))
                           (c-quiesce c))))

(defun c-quiesce (c)
  (typecase c
    (cell 
     (trc nil "c-quiesce unlinking" c)
     (c-unlink-from-used c)
     (when (typep c 'cell)
       (dolist (user (c-users c))
         (c-unlink-user c user)))
      (trc nil "cell quiesce nulled cell awake" c))))

(defmethod not-to-be (other)
  other)

(defparameter *to-be-dbg* nil)

(defmacro make-kid (class &rest initargs)
  `(make-instance ,class
     :fm-parent (progn (assert self) self)
     ,@initargs))

