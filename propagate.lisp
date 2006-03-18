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
  (trc nil  "c-pulse-update updating" *data-pulse-id* c key)
  (setf (c-changed c) nil
      (c-pulse c) *data-pulse-id*))

;--------------- propagate  ----------------------------


; n.b. the cell argument may have been optimized away,
; though it is still receiving final processing here.
;

(defun c-propagate (c prior-value prior-value-supplied)

  (count-it :c-propagate)
  
  (let (*c-calculators* 
        (*c-prop-depth*  (1+ *c-prop-depth*))
        (*defer-changes* t))
    (trc nil "c-propagate clearing *c-calculators*" c)

    ;------ debug stuff ---------
    ;
    (when *stop*
      (princ #\.)(princ #\!)
      (return-from c-propagate))    
    (trc nil "c-propagate> propping" c (c-value c) :user-ct (length (c-users c)) c)
    
    (when *c-debug*
      (when (> *c-prop-depth* 250)
        (trc nil "c-propagate deep" *c-prop-depth* (c-model c) (c-slot-name c) #+nah c))
      (when (> *c-prop-depth* 300)
        (c-break "c-propagate looping ~c" c)))
    
    ; --- manifest new value as needed ---
    ;
    ; propagation to users jumps back in front of client slot-value-observe handling in cells3
    ; because model adopting (once done by the kids change handler) can now be done in
    ; shared-initialize (since one is now forced to supply the parent to make-instance).
    ;
    ; we wnat it here to support (eventually) state change rollback. change handlers are
    ; expected to have side-effects, so we want to propagate fully and be sure no rule
    ; wants a rollback before starting with the side effects.
    ; 
    (c-propagate-to-users c)

    (slot-value-observe (c-slot-name c) (c-model c)
      (c-value c) prior-value prior-value-supplied)
    ;
    ; with propagation done, ephemerals can be reset. we also do this in c-awaken, so
    ; let the fn decide if C really is ephemeral. Note that it might be possible to leave
    ; this out and use the datapulse to identify obsolete ephemerals and clear them
    ; when read. That would avoid ever making again bug I had in which I had the reset inside slot-value-observe,
    ; thinking that that always followed propagation to users. It would also make
    ; debugging easier in that I could find the last ephemeral value in the inspector.
    ; would this be bad for persistent CLOS, in which a DB would think there was still a link
    ; between two records until the value actually got cleared?
    ;
    (c-ephemeral-reset c)
    ))

; --- slot change -----------------------------------------------------------

(defmacro defobserver (slotname
                      (&optional (self-arg 'self) (new-varg 'new-value)
                                 (oldvarg 'old-value) (oldvargboundp 'old-value-boundp))
                      &body output-body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',slotname :output-defined) t))
     ,(if (eql (last1 output-body) :test)
          (let ((temp1 (gensym))
                (loc-self (gensym)))
            `(defmethod slot-value-observe #-(or clisp cormanlisp) progn ((slotname (eql ',slotname)) ,self-arg ,new-varg ,oldvarg ,oldvargboundp)
               (let ((,temp1 (bump-output-count ,slotname))
                     (,loc-self ,(if (listp self-arg)
                                     (car self-arg)
                                   self-arg)))
                 (when (and ,oldvargboundp ,oldvarg)
                   (format t "~&output ~d (~a ~a) old: ~a" ,temp1 ',slotname ,loc-self ,oldvarg))
                 (format t "~&output ~d (~a ~a) new: ~a" ,temp1 ',slotname ,loc-self ,new-varg))))
        `(defmethod slot-value-observe
             #-(or clisp cormanlisp) progn ;;broke cells-gtk #+(or clisp cormanlisp) :around
           ((slotname (eql ',slotname)) ,self-arg ,new-varg ,oldvarg ,oldvargboundp)
           (declare (ignorable
                     ,@(flet ((arg-name (arg-spec)
                                (etypecase arg-spec
                                  (list (car arg-spec))
                                  (atom arg-spec))))
                         (list (arg-name self-arg)(arg-name new-varg)
                           (arg-name oldvarg)(arg-name oldvargboundp)))))
           ,@output-body
           ;;broke cells-gtk #+(or clisp cormanlisp) (call-next-method)
           ))))

(defmacro bump-output-count (slotname) ;; pure test func
  `(if (get ',slotname :outputs)
       (incf (get ',slotname :outputs))
     (setf (get ',slotname :outputs) 1)))

; --- recalculate dependents ----------------------------------------------------

(defun c-propagate-to-users (c)
  ;
  ;  We must defer propagation to users because of an edge case in which:
  ;    - X tells A to recalculate
  ;    - A asks B for its current value
  ;    - B must recalculate because it too uses X
  ;    - if B propagates to its users after recalculating instead of deferring it
  ;       - B might tell H to reclaculate, where H decides this time to use A
  ;       - but A is in the midst of recalculating, and cannot complete until B returns.
  ;         but B is busy eagerly propagating. "This time" is important because it means
  ;         there is no way one can reliably be sure H will not ask for A
  ;
  (trc nil "c-propagate-to-users > queueing" c)
  (when (c-users c)
    (let ((causation (cons c *causation*))) ;; in case deferred
      (with-integrity (:tell-dependents c)
        (assert (null *c-calculators*))
        (let ((*causation* causation))
          (trc nil "c-propagate-to-users > notifying users of" c)
          (dolist (user (c-users c))
            (unless (member (cr-lazy user) '(t :always :once-asked))
              (trc nil "propagating to user is (used,user):" c user)
              (c-value-ensure-current user :user-propagation))))))))




