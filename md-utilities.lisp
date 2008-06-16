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
    (loop for slot-name in (md-owning-slots self)
        do (not-to-be (slot-value self slot-name))))

  (:method :around ((self model-object))
    (declare (ignorable self))
    (let ((*not-to-be* t)
          (dbg nil))
      
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
                           (c-quiesce c)))
  (when (register? self)
    (fm-check-out self)))

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

(export! self-owned self-owned?)

(defun (setf self-owned) (new-value self thing)
  (if (consp thing)
      (loop for e in thing do
            (setf (self-owned self e) new-value))
    (if new-value
        (progn
          (assert (not (find thing (z-owned self))))
          (push thing (z-owned self)))
      (progn
        (assert (find thing (z-owned self)))
        (setf (z-owned self)(delete thing (z-owned self)))))))

(defun self-owned? (self thing)
  (find thing (z-owned self)))

(defvar *c-d-d*)
(defvar *max-d-d*)


(defun count-model (self)
  (setf *c-d-d* (make-hash-table :test 'eq) *max-d-d* 0)
  (with-metrics (t nil "cells statistics for" self)
    (labels ((cc (self)
               (count-it :thing)
               (count-it :thing (type-of self))
               ;(count-it :thing-type (type-of self))
               (loop for (id . c) in (cells self)
                   do (count-it :live-cell)
                     ;(count-it :live-cell id)

                     (typecase c
                       (c-dependent
                        (count-it :dependent-cell)
                        (loop repeat (length (c-useds c))
                            do (count-it :cell-useds)
                              (count-it :dep-depth (c-depend-depth c))))
                       (otherwise (if (c-inputp c)
                                      (count-it :c-input id)
                                    (count-it :c-unknow))))
                     
                     (loop repeat (length (c-callers c))
                         do (count-it :cell-callers)))
               
               (loop repeat (length (cells-flushed self))
                   do (count-it :flushed-cell #+toomuchinfo id))
               
               (loop for slot in (md-owning-slots self) do
                     (loop for k in (let ((sv (SLOT-VALUE self slot)))
                                      (if (listp sv) sv (list sv)))
                         do (cc k)))))
      (cc self))))

(defun c-depend-depth (ctop)
  (if (null (c-useds ctop))
      0
    (or (gethash ctop *c-d-d*)
      (labels ((cdd (c &optional (depth 1) chain)
                 (when (and (not (c-useds c))
                         (> depth *max-d-d*))
                   (setf *max-d-d* depth)
                   (trc "new dd champ from user"  depth :down-to c)
                   (when (= depth 41)
                     (trc "end at" (c-slot-name c) :of (type-of (c-model c)))
                     (loop for c in chain do
                           (trc "called by" (c-slot-name c) :of (type-of (c-model c))))))
                 (setf (gethash c *c-d-d*)
                   ;(break "c-depend-depth ~a" c)
                   (progn
                     ;(trc "dd" c)
                     (1+ (loop for u in (c-useds c)
                             maximizing (cdd u (1+ depth) (cons c chain))))))))
        (cdd ctop)))))
    