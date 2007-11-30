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

;----------------- change detection ---------------------------------

(defun c-no-news (c new-value old-value)
  ;;; (trc nil "c-no-news > checking news between" newvalue oldvalue)
  (bif (test (c-unchanged-test (c-model c) (c-slot-name c)))
      (funcall test new-value old-value)
      (eql new-value old-value)))

(defmacro def-c-unchanged-test ((class slotname) &body test)
  `(defmethod c-unchanged-test ((self ,class) (slotname (eql ',slotname)))
     ,@test))
     
(defmethod c-unchanged-test (self slotname)
  (declare (ignore self slotname))
  nil)

; --- data pulse (change ID) management -------------------------------------

(defun data-pulse-next (pulse-info)
  (declare (ignorable pulse-info))
  (trc nil "data-pulse-next > " (1+ *data-pulse-id*) pulse-info)
  (incf *data-pulse-id*))

(defun c-currentp (c)
  (eql (c-pulse c) *data-pulse-id*))

(defun c-pulse-update (c key)
  (declare (ignorable key))
  (unless (find key '(:valid-uninfluenced))
    (trc nil "!!!!!!! c-pulse-update updating !!!!!!!!!!" *data-pulse-id* c key :prior-pulse (c-pulse c)))
  (assert (>= *data-pulse-id* (c-pulse c)) ()
    "Current DP ~a not GE pulse ~a of cell ~a" *data-pulse-id* (c-pulse c) c)
  (setf (c-pulse c) *data-pulse-id*))

;--------------- propagate  ----------------------------


; n.b. the cell argument may have been optimized away,
; though it is still receiving final processing here.
;

(defun c-propagate (c prior-value prior-value-supplied)
  
  (count-it :c-propagate)
  (when prior-value
    (assert prior-value-supplied () "How can prior-value-supplied be nil if prior-value is not?!! ~a" c))
  (let (*call-stack* 
        (*c-prop-depth*  (1+ *c-prop-depth*))
        (*defer-changes* t))
    (trc nil "c-propagate clearing *call-stack*" c)
    
    ;------ debug stuff ---------
    ;
    (when *stop*
      (princ #\.)(princ #\!)
      (return-from c-propagate))    
    (trc nil  "c-propagate> !!!!!!! propping" c (c-value c) :caller-ct (length (c-callers c)))
    #+slow (trc c "c-propagate> !!!! new value" (c-value c) :prior-value prior-value :caller-ct (length (c-callers c)) c)
    (when *c-debug*
      (when (> *c-prop-depth* 250)
        (trc nil "c-propagate deep" *c-prop-depth* (c-model c) (c-slot-name c) #+nah c))
      (when (> *c-prop-depth* 300)
        (c-break "c-propagate looping ~c" c)))
    
    ; --- manifest new value as needed ---
    ;
    ; 20061030 Trying not.to.be first because doomed instances may be interested in callers
    ; who will decide to propagate. If a family instance kids slot is changing, a doomed kid
    ; will be out of the kids but not yet quiesced. If the propagation to this rule asks the kid
    ; to look at its siblings (say a view instance being deleted from a stack who looks to the psib
    ; pb to decide its own pt), the doomed kid will still have a parent but not be in its kids slot
    ; when it goes looking for a sibling relative to its position.
    ;
    (when (and prior-value-supplied
            prior-value
            (md-slot-owning (type-of (c-model c)) (c-slot-name c)))
      (trc nil "c-propagate> contemplating lost")
      (flet ((listify (x) (if (listp x) x (list x))))
        (bif (lost (set-difference (listify prior-value) (listify (c-value c))))
          (progn
            (trc nil "prop nailing owned!!!!!!!!!!!" c :lost lost :leaving (c-value c))
            (mapcar 'not-to-be lost))
          (trc nil "no owned lost!!!!!"))))
    
    ; propagation to callers jumps back in front of client slot-value-observe handling in cells3
    ; because model adopting (once done by the kids change handler) can now be done in
    ; shared-initialize (since one is now forced to supply the parent to make-instance).
    ;
    ; we wnat it here to support (eventually) state change rollback. change handlers are
    ; expected to have side-effects, so we want to propagate fully and be sure no rule
    ; wants a rollback before starting with the side effects.
    ; 
    (unless nil #+not (member (c-lazy c) '(t :always :once-asked)) ;; 2006-09-26 still fuzzy on this 
      (c-propagate-to-callers c))
    
    (slot-value-observe (c-slot-name c) (c-model c)
      (c-value c) prior-value prior-value-supplied)
    
    
    ;
    ; with propagation done, ephemerals can be reset. we also do this in c-awaken, so
    ; let the fn decide if C really is ephemeral. Note that it might be possible to leave
    ; this out and use the datapulse to identify obsolete ephemerals and clear them
    ; when read. That would avoid ever making again bug I had in which I had the reset inside slot-value-observe,
    ; thinking that that always followed propagation to callers. It would also make
    ; debugging easier in that I could find the last ephemeral value in the inspector.
    ; would this be bad for persistent CLOS, in which a DB would think there was still a link
    ; between two records until the value actually got cleared?
    ;
    (ephemeral-reset c)))

; --- slot change -----------------------------------------------------------

(defmacro defobserver (slotname &rest args &aux (aroundp (eq :around (first args))))
  (when aroundp (setf args (cdr args)))
  (destructuring-bind ((&optional (self-arg 'self) (new-varg 'new-value)
                         (oldvarg 'old-value) (oldvargboundp 'old-value-boundp))
                       &body output-body) args
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',slotname :output-defined) t))
       ,(if (eql (last1 output-body) :test)
            (let ((temp1 (gensym))
                  (loc-self (gensym)))
              `(defmethod slot-value-observe #-(or cormanlisp) ,(if aroundp :around 'progn)
                 ((slotname (eql ',slotname)) ,self-arg ,new-varg ,oldvarg ,oldvargboundp)
                 (let ((,temp1 (bump-output-count ,slotname))
                       (,loc-self ,(if (listp self-arg)
                                       (car self-arg)
                                     self-arg)))
                   (when (and ,oldvargboundp ,oldvarg)
                     (format t "~&output ~d (~a ~a) old: ~a" ,temp1 ',slotname ,loc-self ,oldvarg))
                   (format t "~&output ~d (~a ~a) new: ~a" ,temp1 ',slotname ,loc-self ,new-varg))))
          `(defmethod slot-value-observe
               #-(or cormanlisp) ,(if aroundp :around 'progn)
             ((slotname (eql ',slotname)) ,self-arg ,new-varg ,oldvarg ,oldvargboundp)
             (declare (ignorable
                       ,@(flet ((arg-name (arg-spec)
                                  (etypecase arg-spec
                                    (list (car arg-spec))
                                    (atom arg-spec))))
                           (list (arg-name self-arg)(arg-name new-varg)
                             (arg-name oldvarg)(arg-name oldvargboundp)))))
             ,@output-body)))))

(defmacro bump-output-count (slotname) ;; pure test func
  `(if (get ',slotname :outputs)
       (incf (get ',slotname :outputs))
     (setf (get ',slotname :outputs) 1)))

; --- recalculate dependents ----------------------------------------------------



(defun c-propagate-to-callers (c)
  ;
  ;  We must defer propagation to callers because of an edge case in which:
  ;    - X tells A to recalculate
  ;    - A asks B for its current value
  ;    - B must recalculate because it too uses X
  ;    - if B propagates to its callers after recalculating instead of deferring it
  ;       - B might tell H to reclaculate, where H decides this time to use A
  ;       - but A is in the midst of recalculating, and cannot complete until B returns.
  ;         but B is busy eagerly propagating. "This time" is important because it means
  ;         there is no way one can reliably be sure H will not ask for A
  ;
  (when (find-if-not (lambda (caller)
                       (and (c-lazy caller) ;; slight optimization
                         (member (c-lazy caller) '(t :always :once-asked))))
          (c-callers c))
    (let ((causation (cons c *causation*))) ;; in case deferred
      #+slow (TRC c "c-propagate-to-callers > queueing notifying callers" (c-callers c))
      (with-integrity (:tell-dependents c)
        (assert (null *call-stack*))
        (let ((*causation* causation))
          (trc nil "c-propagate-to-callers > actually notifying callers of" c (c-callers c))
          #+c-debug (dolist (caller (c-callers c))
                      (assert (find c (cd-useds caller)) () "test 1 failed ~a ~a" c caller))
          #+c-debug (dolist (caller (copy-list (c-callers c))) ;; following code may modify c-callers list...
                      (trc nil "PRE-prop-CHECK " c :caller caller (c-state caller) (c-lazy caller))
                      (unless (or (eq (c-state caller) :quiesced) ;; ..so watch for quiesced
                                (member (c-lazy caller) '(t :always :once-asked)))
                        (assert (find c (cd-useds caller))() "Precheck Caller ~a of ~a does not have it as used" caller c)
                        ))
          (dolist (caller (progn #+not copy-list (c-callers c))) ;; following code may modify c-callers list...
            (trc nil "propagating to caller iterates" c :caller caller (c-state caller) (c-lazy caller))
            (unless (or (eq (c-state caller) :quiesced) ;; ..so watch for quiesced
                      (member (c-lazy caller) '(t :always :once-asked)))
              (assert (find c (cd-useds caller))() "Caller ~a of ~a does not have it as used" caller c)
              #+slow (trc c "propagating to caller is used" c :caller caller (c-currentp c))
              (let ((*trc-ensure* (trcp c)))
                (ensure-value-is-current caller :prop-from c)))))))))




