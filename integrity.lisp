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

(defun data-pulse-next (pulse-info)
  (declare (ignorable pulse-info))
  (trc nil "data-pulse-next > " (1+ *data-pulse-id*) pulse-info)
  (if (< *data-pulse-id* most-positive-fixnum)
      (incf *data-pulse-id*)
    (progn
      (c-break "BINGO!!! gotta handle wrap on *data-pulse-id* ~a" *data-pulse-id*)
      (setf *data-pulse-id* 1)))) ;; nah, this is gonna take some work

(defun c-currentp (c)
  (eql (c-pulse c) *data-pulse-id*))

(defun c-pulse-update (c key)
  (declare (ignorable key))
  (trc nil  "c-pulse-update updating" *data-pulse-id* c key)
  (setf (c-changed c) nil
      (c-pulse c) *data-pulse-id*))


;-----------------------------------------

(defparameter *deference-acknowledged* nil)

(defmacro with-deference (&body body)
  "Wrap around any setf in c-output-slot-name (aka def-c-output) 
to avoid warning that setf is being deferred"
  `(let ((*deference-acknowledged* t))
     ,@body))

(defmacro with-integrity ((debug-key &rest defer-info) &rest body)
  `(call-with-integrity ,debug-key (list ,@defer-info)
     (lambda () ,@body)))

(defun ufb-queue (opcode)
  (cdr (assoc opcode *unfinished-business*)))

(defun ufb-add (opcode continuation)
  (fifo-add (ufb-queue opcode) continuation))

(define-constant *ufb-opcodes* '(:user-notify :output :setf :makunbound :finalize))

(define-condition c-opcode-deferred (c-enabling)
   ((defer-info :initarg :defer-info :reader defer-info))
   (:report
    (lambda (condition stream)
      (format stream "~&Operation ~a deferred till pending business finished"
        (defer-info condition)))))

(defparameter *ufb-needed* nil)

(defun call-with-integrity (debug-key defer-info action &aux (opcode (car defer-info)))
  (declare (ignorable debug-key))
  (assert (or (null opcode) (member opcode *ufb-opcodes*)))
  (trc nil "call-with-integrity entry *unfinished-business*" *unfinished-business*)
  (when *stop*
    (return-from call-with-integrity))
  (if *unfinished-business*
        (if defer-info
            (progn
              (trc nil "call-with-integrity deferring" defer-info)
              (ufb-add opcode (cons (cdr defer-info) action))
              (when (and (not *deference-acknowledged*)
                      (member opcode '(:setf :makunbound)))
                #+not (error 'c-opcode-deferred
                  :defer-info defer-info)
                (trc nil "!!! New pulse, event" *data-pulse-id* defer-info)))
          (funcall action))
    (let ((*unfinished-business*
           (mapcar (lambda (opcode)
                     (cons opcode (make-fifo-queue)))
             *ufb-opcodes*)))
      (trc nil "!!!!!!!!!! started new *unfinished-business*" debug-key defer-info)
      (when (or (zerop *data-pulse-id*)
              (member opcode '(:setf :makunbound)))
        (data-pulse-next (cons opcode defer-info))
        (trc nil "!!! New pulse, event" *data-pulse-id* defer-info))
      (prog1
          (funcall action)
        (unless (find-if-not 'fifo-empty *unfinished-business*)
          (count-it :ufb-wasted))
        (finish-business)))))



(defun finish-business (&aux task some-output setfs (setf-ct 0))
  (declare (ignorable setfs))
  (assert (ufb-queue :user-notify))
  (assert (consp (ufb-queue :user-notify)))
  (tagbody
    notify-users
    ;--- notify users ------------------------------
    (when *stop* (return-from finish-business))
    (let ((user-q-item (fifo-pop (ufb-queue :user-notify))))
       (when user-q-item
         (destructuring-bind (defer-info . task) user-q-item
           (declare (ignorable defer-info))
           (trc nil "finbiz notifying users of cell" (car defer-info))
           (funcall task)
           (go notify-users))))
    
    (setf some-output nil)
    
    next-output
    (when *stop* (return-from finish-business))
    ;--- do c-output-slot-name -----------------------
    (setf task (cdr (fifo-pop (ufb-queue :output))))
    
    (cond
     (task
      (setf some-output t)
      (trc nil "finish-business outputting------------------------")
      (funcall task)
      (go next-output))
     (some-output
      (go notify-users)))
    
    ; --- do deferred setfs ------------------------
    (setf task (fifo-pop (ufb-queue :setf)))
    (when task
      (incf setf-ct)
      (destructuring-bind ((c new-value) . task-fn) task
        (trc nil "finbiz: deferred setf" c new-value)
        (if (find c *causation*)
            (break "setf looping setting ~a to ~a with history ~a" 
              c new-value *causation*)
          (progn
            (push c setfs)
            (data-pulse-next (list :finbiz c new-value))
            (funcall task-fn))))
      (go notify-users))

    ; --- do finalizations ------------------------
    (setf task (fifo-pop (ufb-queue :finalize)))
    (when task
      (destructuring-bind ((self) . task-fn) task
        (trc "finbiz: deferred finalize!!!!" self)
        (funcall task-fn))
      (go notify-users))))
