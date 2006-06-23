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

(defparameter *ide-app-hard-to-kill* nil)

(defun md-slot-value (self slot-name &aux (c (md-slot-cell self slot-name)))
  (tagbody
    retry
    (when *stop*
      (if *ide-app-hard-to-kill*
          (progn
            (princ #\.)
            (return-from md-slot-value))
        (restart-case
            (error "Cells is stopped due to a prior error.")
          (continue ()
            :report "Return a slot value of nil."
            (return-from md-slot-value nil))
          (reset-cells ()
            :report "Reset cells and retry getting the slot value."
            (cells-reset)
            (go retry))))))
  
  ;; (count-it :md-slot-value slot-name)
  (if c
      (prog1
          (with-integrity ()
            (ensure-value-is-current c))
        (when (car *call-stack*)
          (record-caller c)))
    (values (bd-slot-value self slot-name) nil)))
  
(defun ensure-value-is-current (c)
  (count-it :ensure-value-is-current)
  (trc nil "ensure-value-is-current >" c)
  (cond
   ((c-currentp c)(trc nil "c-currentp" c)) ;; used to follow c-inputp, but I am toying with letting ephemerals (inputs) fall obsolete
   ;; and then get reset here (ie, ((c-input-p c) (ephemeral-reset c))). ie, do not assume inputs are never obsolete
   ;;
   ((c-inputp c)(trc nil "c-inputp" c)) ;; always current (for now; see above)

   ((or (not (c-validp c))
      (some (lambda (used)
              (ensure-value-is-current used)
              (trc nil "comparing pulses (caller, used): " (c-pulse c)(c-pulse used))
              (when (and (c-changed used) (> (c-pulse used)(c-pulse c)))
                 (trc nil "used changed" c used)
                t))
        (cd-useds c)))
    (trc nil "ensuring current calc-set of" (c-slot-name c) debug-id)
    (calculate-and-set c))

   (t (c-pulse-update c :valid-uninfluenced)))

  (when (c-unboundp c)
    (error 'unbound-cell :cell c :instance (c-model c) :name (c-slot-name c)))

  (c-value c))

(defun calculate-and-set (c)
  (flet ((body ()
           (when (c-stopped)
             (princ #\.)
             (return-from calculate-and-set))
           
           (when (find c *call-stack*) ;; circularity
             (c-break ;; break is problem when testing cells on some CLs
              "cell ~a midst askers: ~a" c *call-stack*))
  
           (multiple-value-bind (raw-value propagation-code)
               (calculate-and-link c)
             
             (when (and *c-debug* (typep raw-value 'cell))
               (c-break "new value for cell ~s is itself a cell: ~s. probably nested (c? ... (c? ))"
                 c raw-value))
             
             (md-slot-value-assume c raw-value propagation-code))))
    (if nil ;; *dbg*
        (ukt::wtrc (0 100 "calcnset" c) (body))
      (body))))

(defun calculate-and-link (c)
  (let ((*call-stack* (cons c *call-stack*))
        (*defer-changes* t))
    (cd-usage-clear-all c)
    (multiple-value-prog1
        (funcall (cr-rule c) c)
      (c-unlink-unused c))))

;-------------------------------------------------------------

(defun md-slot-makunbound (self slot-name
                            &aux (c (md-slot-cell self slot-name)))
  (unless c
    (c-break ":md-slot-makunbound > cellular slot ~a of ~a cannot be unbound unless initialized as inputp"
      slot-name self))
  
  (when (c-unboundp c)
    (return-from md-slot-makunbound nil))

  (when *within-integrity* ;; 2006-02 oops, bad name
    (c-break "md-slot-makunbound of ~a must be deffered by wrapping code in with-integrity" c))
  
  ; 
  ; Big change here for Cells III: before, only the propagation was deferred. Man that seems
  ; wrong. So now the full makunbound processing gets deferred. Less controversially,
  ; by contrast the without-c-dependency wrapped everything, and while that is harmless,
  ; it is also unnecessary and could confuse people trying to follow the logic.
  ;
  (let ((causation *causation*))
    (with-integrity (:change c)
      (let ((*causation* causation))
        ; --- cell & slot maintenance ---
        (let ((prior-value (c-value c)))
          (setf (c-value-state c) :unbound
            (c-value c) nil
            (c-state c) :awake)
          (bd-slot-makunbound self slot-name)
          ;
          ; --- data flow propagation -----------
          ;
          (setf (c-changed c) t)
          (without-c-dependency
              (c-propagate c prior-value t)))))))

;;; --- setf md.slot.value --------------------------------------------------------
;;;

(defun (setf md-slot-value) (new-value self slot-name
                              &aux (c (md-slot-cell self slot-name)))
  
  (when *c-debug*
    (c-setting-debug self slot-name c new-value))
  
  (unless c
    (c-break "cellular slot ~a of ~a cannot be SETFed because it is not 
mediated by a Cell with :inputp t. To achieve this, the initial value ~s -- whether 
supplied as an :initform, :default-initarg, or at make-instance time via 
an :initarg -- should be wrapped in either macro C-IN or C-INPUT. 
In brief, initialize ~0@*~a to (c-in ~2@*~s) instead of plain ~:*~s"
      slot-name self (slot-value self slot-name)))

  (when *defer-changes*
    (c-break "SETF of ~a must be deferred by wrapping code in WITH-INTEGRITY" c))

  (with-integrity (:change)
    (md-slot-value-assume c new-value nil))

  ;; new-value 
  ;; above line commented out 2006-05-01. It seems to me we want the value assumed by the slot
  ;; not the value setf'ed (on rare occasions they diverge, or at least used to for delta slots)
  ;; anyway, if they no longer diverge the question of which to return is moot
  )
                    
(defmethod md-slot-value-assume (c raw-value propagation-code)
  (assert c)
  (without-c-dependency
      (let ((prior-state (c-value-state c))
            (prior-value (c-value c))
            (absorbed-value (c-absorb-value c raw-value)))

        (c-pulse-update c :slotv-assume)

        ; --- head off unchanged; this got moved earlier on 2006-06-10 ---
        (when (and (not (eq propagation-code :propagate))
                (eql prior-state :valid)
                (c-no-news c absorbed-value prior-value))
          (trc nil "(setf md-slot-value) > early no news" propagation-code prior-state prior-value  absorbed-value)
          (count-it :nonews)
          (return-from md-slot-value-assume absorbed-value))

        ; --- slot maintenance ---
        (unless (c-synaptic c)
          (md-slot-value-store (c-model c) (c-slot-name c) absorbed-value))
        
        ; --- cell maintenance ---
        (setf
         (c-value c) absorbed-value
         (c-value-state c) :valid
         (c-state c) :awake)
        
        (c-optimize-away?! c) ;;; put optimize test here to avoid needless linking
        
        ; --- data flow propagation -----------
        (unless (eq propagation-code :no-propagate)
          (setf (c-changed c) t)
          (c-propagate c prior-value (eq prior-state :valid)))  ;; until 06-02-13 was (not (eq prior-state :unbound))
        
        absorbed-value)))


    
