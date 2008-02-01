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

(define-constant *ufb-opcodes* '(:tell-dependents
                                 :awaken
                                 :client
                                 :ephemeral-reset
                                 :change))

(defmacro with-integrity ((&optional opcode defer-info debug) &rest body)
  (when opcode
    (assert (find opcode *ufb-opcodes*) ()
      "Invalid opcode for with-integrity: ~a. Allowed values: ~a" opcode *ufb-opcodes*))
  `(call-with-integrity ,opcode ,defer-info (lambda (opcode defer-info)
                                              (declare (ignorable opcode defer-info))
                                              ,(when debug
                                                `(trc "integrity action entry" opcode defer-info ',body))
                                              ,@body)))

(export! with-cc)

(defmacro with-cc (id &body body)
  `(with-integrity (:change ,id)
     ,@body))

(defun integrity-managed-p ()
  *within-integrity*)

(defun call-with-integrity (opcode defer-info action)
  (when *stop*
    (return-from call-with-integrity))
  (if *within-integrity*
      (if opcode
          (ufb-add opcode (cons defer-info action))
        (funcall action opcode defer-info))
    (let ((*within-integrity* t)
          *unfinished-business*
          *defer-changes*)
      (trc nil "initiating new UFB!!!!!!!!!!!!" opcode defer-info)
      (when (or (zerop *data-pulse-id*)
              (eq opcode :change))
        (eko (nil "!!! New pulse, event" *data-pulse-id* defer-info)
          (data-pulse-next (cons opcode defer-info))))
      (prog1
          (funcall action opcode defer-info)
        (finish-business)))))

(defun ufb-queue (opcode)
  (assert (find opcode *ufb-opcodes*))
  (cdr (assoc opcode *unfinished-business*)))

(defun ufb-queue-ensure (opcode)
  (assert (find opcode *ufb-opcodes*))
  (or (ufb-queue opcode)
    (cdr (car (push (cons opcode (make-fifo-queue)) *unfinished-business*)))))

(defparameter *no-tell* nil)

(defun ufb-add (opcode continuation)
  (assert (find opcode *ufb-opcodes*))
  #+trythis (when (and *no-tell* (eq opcode :tell-dependents))
    (break "truly queueing tell under no-tell"))
  (trc nil "ufb-add deferring" opcode (when (eql opcode :client)(car continuation)))
  (fifo-add (ufb-queue-ensure opcode) continuation))

(defun just-do-it (op-or-q &aux (q (if (keywordp op-or-q)
                                       (ufb-queue op-or-q)
                                     op-or-q)))
  (trc nil "----------------------------just do it doing---------------------" op-or-q)
  (loop for (defer-info . task) = (fifo-pop q)
        while task
        do (trc nil "unfin task is" opcode task)
          (funcall task op-or-q defer-info)))

(defun finish-business ()
  (when *stop* (return-from finish-business))
  (tagbody
    tell-dependents
    (just-do-it :tell-dependents)
    ;
    ; while the next step looks separate from the prior, they are closely bound.
    ; during :tell-dependents, any number of new model instances can be spawned.
    ; as they are spawned, shared-initialize queues them for awakening, which
    ; you will recall forces the calculation of ruled cells and observer notification
    ; for all cell slots. These latter may enqueue :change or :client tasks, in which
    ; case note that they become appended to :change or :client tasks enqueued
    ; during :tell-dependents. How come? Because the birth itself of model instances during
    ; a datapulse is considered part of that datapulse, so we do want tasks enqueued
    ; during their awakening to be handled along with those enqueued by cells of
    ; existing model instances.
    ;
    (bwhen (uqp (fifo-peek (ufb-queue :tell-dependents)))
      (trcx finish-business uqp)
      (dolist (b (fifo-data (ufb-queue :tell-dependents)))
        (trc "unhandled :tell-dependents" (car b) (c-callers (car b))))
      (break "unexpected 1> ufb needs to tell dependnents after telling dependents"))
    (let ((*no-tell* t))
      (just-do-it :awaken) ;--- md-awaken new instances ---
      )
    ;
    ; OLD THINKING, preserved for the record, but NO LONGER TRUE:
    ;  we do not go back to check for a need to :tell-dependents because (a) the original propagation
    ; and processing of the :tell-dependents queue is a full propagation; no rule can ask for a cell that
    ; then decides it needs to recompute and possibly propagate; and (b) the only rules forced awake during
    ; awakening need that precisely because no one asked for their values, so there can be no dependents
    ; to "tell". I think. :) So...
    ; END OF OLD THINKING
    ;
    ; We now allow :awaken to change things so more dependents need to be told. The problem is the implicit 
    ; dependence on the /life/ of a model whenever there is a dependence on any /cell/ of that model. 
    ; md-quiesce currently just flags such slots as uncurrent -- maybe /that/ should change and those should 
    ; recalculate at once -- and then an /observer/ can run and ask for a new value from such an uncurrent cell, 
    ; which now knows it must recalculate. And that recalculation of course can and likely will come up with a new value
    ; and perforce need to tell its dependents. So...
    ;
    ; I /could/ explore something other than the "uncurrent" kludge, but NCTM 2007 is coming up and
    ; to be honest the idea of not allowing nested tells was enforcing a /guess/ that that should not
    ; arise, and there was not even any perceived integrity whole being closed, it was just a gratuitous
    ; QA trick, and indeed for a long time many nested tells were avoidable. But the case of the quiesced
    ; dependent reverses the arrow and puts the burden on the prosecution to prove nested tells are a problem.
    
    (bwhen (uqp (fifo-peek (ufb-queue :tell-dependents)))
      (trc "retelling dependenst, one new one being" uqp)
      (go tell-dependents))
    
    ;--- process client queue ------------------------------
    ;
    (when *stop* (return-from finish-business))
    
    handle-clients
    (bwhen (clientq (ufb-queue :client))
      (if *client-queue-handler*
          (funcall *client-queue-handler* clientq) ;; might be empty/not exist, so handlers must check
        (just-do-it clientq))
      (when (fifo-peek (ufb-queue :client))
        #+shhh (ukt::fifo-browse (ufb-queue :client) (lambda (entry)
                                                       (trc "surprise client" entry)))
        (go handle-clients)))
    ;--- now we can reset ephemerals --------------------
    ;
    ; one might be wondering when the observers got notified. That happens
    ; Nice historical note: by accident, in the deep-cells test to exercise the new behavior
    ; of cells3, I coded an ephemeral cell and initialized it to non-nil, hitting a runtime
    ; error (now gone) saying I had no idea what a non-nil ephemeral would mean. That had been
    ; my conclusion when the idea occurred to me the first time, so I stuck in an assertion
    ; to warn off callers. 
    ;
    ; But the new
    ; datachange progression defined by Cells3 had already forced me to manage ephemeral resets
    ; more predictably (something in the test suite failed). By the time I got the runtime
    ; error on deep-cells I was able to confidently take out the error and just let the thing
    ; run. deep-cells looks to behave just right, but maybe a tougher test will present a problem?
    ;
    (just-do-it :ephemeral-reset)
    
    ;--- do deferred state changes -----------------------
    ;
    (bwhen (task-info (fifo-pop (ufb-queue :change)))
      (trc nil "!!! finbiz --- CHANGE ---- (first of)" (fifo-length (ufb-queue :change)))
      (destructuring-bind (defer-info . task-fn) task-info
        (trc nil  "finbiz: deferred state change" defer-info)
        (data-pulse-next (list :finbiz defer-info))
        (funcall task-fn :change defer-info)
        ;
        ; to finish this state change we could recursively call (finish-business), but
        ; a goto let's us not use the stack. Someday I envision code that keeps on
        ; setf-ing, polling the OS for events, in which case we cannot very well use
        ; recursion. But as a debugger someone might want to change the next form
        ; to (finish-business) if they are having trouble with a chain of setf's and
        ; want to inspect the history on the stack.
        ;
        (go tell-dependents)))))


