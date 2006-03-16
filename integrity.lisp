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

(define-constant *ufb-opcodes* '(:tell-dependents
                                 :awaken
                                 :client
                                 :ephemeral-reset
                                 :change))

(defmacro with-integrity ((&optional opcode defer-info) &rest body)
  (when opcode
    (assert (find opcode *ufb-opcodes*) ()
            "Invalid second value to with-integrity: ~a" opcode))
  `(call-with-integrity ,opcode ,defer-info (lambda () ,@body)))

(defun integrity-managed-p ()
  *within-integrity*)

(defun call-with-integrity (opcode defer-info action)
  (when *stop*
    (return-from call-with-integrity))
  (if *within-integrity*
        (if opcode
            (ufb-add opcode (cons defer-info action))
          (funcall action))
    (let ((*within-integrity* t)
          *unfinished-business*)
      (when (or (zerop *data-pulse-id*)
              (eq opcode :change))
        (eko (nil "!!! New pulse, event" *data-pulse-id* defer-info)
          (data-pulse-next (cons opcode defer-info))))
      (prog1
          (funcall action)
        (finish-business)))))

(defun ufb-queue (opcode)
  (assert (find opcode *ufb-opcodes*))
  (cdr (assoc opcode *unfinished-business*)))

(defun ufb-queue-ensure (opcode)
  (assert (find opcode *ufb-opcodes*))
  (or (ufb-queue opcode)
    (cdr (car (push (cons opcode (make-fifo-queue)) *unfinished-business*)))))

(defun ufb-add (opcode continuation)
  (assert (find opcode *ufb-opcodes*))
  (fifo-add (ufb-queue-ensure opcode) continuation))

(defun just-do-it (op-or-q &aux (q (if (keywordp op-or-q)
                                       (ufb-queue op-or-q)
                                     op-or-q)))
  (loop for (nil . task) = (fifo-pop q)
        while task
        do (trc nil "unfin task is" opcode task)
          (funcall task)))

(defun finish-business ()
  (when *stop* (return-from finish-business))
  (tagbody
    tell-dependents
    (just-do-it :tell-dependents)

    (just-do-it :awaken) ;--- awaken new instances ---

    ;--- process client queue ------------------------------
    ;
    (when *stop* (return-from finish-business))
    (trc (fifo-peek (ufb-queue :client)) "!!! finbiz --- USER --- length" (fifo-length (ufb-queue :client)))

    (bwhen (clientq (ufb-queue :client))
      (if *client-queue-handler*
          (funcall *client-queue-handler* clientq) ;; might be empty/not exist
        (just-do-it clientq)))

    ;--- now we can reset ephemerals --------------------
    (just-do-it :ephemeral-reset)
    
    ;--- do deferred state changes -----------------------
    ;
    (bwhen (task-info (fifo-pop (ufb-queue :change))) ;; it would be odd, but nils can legally inhabit queues, so be safe...
      (trc nil "!!!!!!!!!!!!!!!!!!! finbiz --- CHANGE ---- (first of)" (fifo-length (ufb-queue :change)))
      (destructuring-bind (defer-info . task-fn) task-info
        (trc nil "finbiz: deferred state change" defer-info)
        (data-pulse-next (list :finbiz defer-info))
        (funcall task-fn)
        (go tell-dependents)))))

