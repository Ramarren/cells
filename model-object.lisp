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

;;; --- model-object ----------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
    (export '(md-name fm-parent .parent)))

(defclass model-object ()
  ((.md-state :initform :nascent :accessor md-state) ; [nil | :nascent | :alive | :doomed]
   (.cells :initform nil :accessor cells)
   (.cells-flushed :initform nil :accessor cells-flushed
                   :documentation "cells supplied but un-whenned or optimized-away")
   (adopt-ct :initform 0 :accessor adopt-ct)))

;;; --- md obj initialization ------------------

(defmethod shared-initialize :after ((self model-object) slotnames
                                      &rest initargs &key fm-parent)
  (declare (ignorable initargs slotnames fm-parent))
  ;
  ; for convenience and transparency of mechanism we allow client code 
  ; to intialize a slot to a cell, but we want the slot to hold the functional
  ; value, partly for ease of inspection, partly for performance, mostly
  ; because sometimes we are a slave to other libraries, such as a persistence
  ; library that does interesting things automatically based on the slot value.
  ;
  ; here we shuttle cells out of the slots and into a per-instance dictionary of cells,
  ; as well as tell the cells what slot and instance they are mediating.
  ;
  (loop for esd in (class-slots (class-of self))
      for sn = (slot-definition-name esd)
      for sv = (when (slot-boundp self sn)
                 (slot-value self sn))
      when (typep sv 'cell)
      do (if (md-slot-cell-type (type-of self) sn)
             (md-install-cell self sn sv)
           (when *c-debug*
             (trc "warning: cell ~a offered for non-cellular model/slot ~a/~a" sv self sn))))
  ;
  ; queue up for awakening
  ;
  (with-integrity (:awaken self)
    (md-awaken self)))

(defun md-install-cell (self sn c &aux (c-isa-cell (typep c 'cell)))
  ;
  ; iff cell, init and move into dictionary
  ;
  (when c-isa-cell
    (count-it :md-install-cell)
    (setf
     (c-model c) self
     (c-slot-name c) sn
     (md-slot-cell self sn) c))
  ;
  ; now have the slot really be the slot
  ;
  (if c-isa-cell
      (if (c-unboundp c)
          (bd-slot-makunbound self sn)
        (setf (slot-value self sn)
          (if (c-inputp c)
                  (c-value c)
                nil)))
    (setf (slot-value self sn) c))) ;; (in which case "c" is not actually a cell)

;;; --- awaken --------
;
; -- do initial evaluation of all ruled slots
; -- call observers of all slots

(defmethod md-awaken :around ((self model-object))
  (when (eql :nascent (md-state self))
    (call-next-method)))

(defmethod md-awaken ((self model-object))
  ;
  ; --- debug stuff
  ;
  (when *stop*
    (princ #\.)
    (return-from md-awaken))
  (trc nil "md-awaken entry" self (md-state self))
  (c-assert (eql :nascent (md-state self)))
  (count-it :md-awaken)
  ;;(count-it 'mdawaken (type-of self))
  
  ; ---

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
        
        (cond
         ((not c)
          ;; all slots must hit any change handlers as instances come into existence to get
          ;; models fully connected to the outside world they are controlling. that
          ;; happens in awaken-cell for slots in fact mediated by cells, but as an
          ;; optimization we allow raw literal values to be specified for a slot, in
          ;; which case heroic measures are needed to get the slot to the change handler
          ;;
          ;; next is an indirect and brittle way to determine that a slot has already been output,
          ;; but I think anything better creates a run-time hit.
          ;;
          (unless (md-slot-cell-flushed self slot-name) ;; slot will have been propagated just after cell was flushed
            (slot-value-observe slot-name self (bd-slot-value self slot-name) nil nil)))

         ((find (c-lazy c) '(:until-asked :always t))
          (trc nil "md-awaken deferring c-awaken since lazy" 
            self esd))

         ((eq :nascent (c-state c))
          (c-assert (c-model c) () "c-awaken sees uninstalled cell" c)
          (c-assert (eq :nascent (c-state c)))
          (trc nil "c-awaken > awakening" c)
          (count-it :c-awaken)
                
          (setf (c-state c) :awake)
          (awaken-cell c))))))
  
  (setf (md-state self) :awake)
  self)
  
;;; --- utilities, accessors, etc --------------------------------------

(defmethod c-slot-value ((self model-object) slot)
  (slot-value self slot))

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
  (trc nil "md-slot-value-store" slot-name new-value)
  (setf (slot-value self slot-name) new-value))

(defun md-slot-cell-flushed (self slot-name)
  (cdr (assoc slot-name (cells-flushed self))))

;----------------- navigation: slot <> initarg <> esd <> cell -----------------

#+cmu
(defmethod c-class-name ((class pcl::standard-class))
  (pcl::class-name class))

(defmethod c-class-name (other) (declare (ignore other)) nil)

;; why not #-cmu?
(defmethod c-class-name ((class standard-class))
  (class-name class))

(defmethod cell-when (other) (declare (ignorable other)) nil)

(defun (setf md-slot-cell) (new-cell self slot-name)
  (bif (entry (assoc slot-name (cells self)))
    (let ((old (cdr entry))) ;; s/b being supplanted by kid-slotter
      (declare (ignorable old))
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
