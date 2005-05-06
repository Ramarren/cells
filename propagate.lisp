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

;--------------- propagate  ----------------------------


; n.b. the cell argument may have been optimized away,
; though it is still receiving final processing here.
;

(defun c-propagate (c prior-value prior-value-supplied)

  (count-it :c-propagate)
  
  (let (*c-calculators* 
        (*c-prop-depth*  (1+ *c-prop-depth*)))
    (trc nil "c-propagate clearing *c-calculators*" c)

    ;------ debug stuff ---------
    ;
    (when *stop*
      (princ #\.)(princ #\!)
      (return-from c-propagate))    
    (trc nil "c-propagate> propping" c (c-value c) (length (c-users c)) c)
    
    (when *c-debug*
      (when (> *c-prop-depth* 250)
        (trc nil "c-propagate deep" *c-prop-depth* (c-model c) (c-slot-name c) #+nah c))
      (when (> *c-prop-depth* 300)
        (c-break "c-propagate looping ~c" c)))
    
    ; --- manifest new value as needed ---
    ;
    (c-propagate-to-users c)
    (c-output-slot c (c-slot-name c) (c-model c)
      (c-value c) prior-value prior-value-supplied)))

(defun c-propagate-to-users (c)
  (trc nil "c-propagate-to-users > queueing" c :cause *causation*)
  (let ((causation (cons c *causation*))) ;; in case deferred
    (with-integrity (:user-notify :user-notify c)
      (assert (null *c-calculators*))
      (let ((*causation* causation))
        (trc nil "c-propagate-to-users > notifying users of" c)
        (dolist (user (c-users c))
          (bwhen (dead (catch :mdead
                         (trc nil "c-propagate-to-users> *data-pulse-id*, user, c:" *data-pulse-id* user c)
                         (when (c-user-cares user)
                           (c-value-ensure-current user))
                         nil))
            (when (eq dead (c-model c))
              (trc nil "!!! aborting further user prop of dead" dead)
              (return-from c-propagate-to-users))
            (trc nil "!!! continuing user prop following: user => dead" user dead)))))))

(defun c-user-cares (c)
  (not (or (c-currentp c)
         (member (cr-lazy c) '(t :always :once-asked)))))

(defun c-output-defined (slot-name)
  (getf (symbol-plist slot-name) :output-defined))

(defun c-output-slot (c slot-name self new-value prior-value prior-value-supplied)
  (let ((causation *causation*)) ;; in case deferred
    (with-integrity (:c-output-slot :output c)
      (let ((*causation* causation))
        (trc nil "c-output-slot > causation" c *causation* causation)
        (trc nil "c-output-slot > now!!" self slot-name new-value prior-value)
        (count-it :output slot-name)
        (c-output-slot-name slot-name
          self
          new-value
          prior-value
          prior-value-supplied)
        (c-ephemeral-reset c)))))

(defun c-ephemeral-reset (c)
    (when c
      (when (c-ephemeral-p c)
        (trc nil "!!!!!!!!!!!!!! c-ephemeral-reset resetting:" c)
        (setf (c-value c) nil)))) ;; good q: what does (setf <ephem> 'x) return? historically nil, but...?

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

(defmethod c-identity-p ((value null)) t)
(defmethod c-identity-p ((value number)) (zerop value))
(defmethod c-identity-p ((value cons))
  ;; this def a little suspect?
  (and (c-identity-p (car value))
       (c-identity-p (cdr value))))


;------------------- re think ---------------------------------

(defmethod cmdead (c)
;  (count-it :cmdead)
  (if (or (null (c-model c)) (consp (c-model c)))
      (not (c-optimized-away-p c)) ;; the other way above condition can be met
    (mdead (c-model c))))

(defmethod cmdead :around (c )
  (when (call-next-method)
    (break "still reaching dead cells ~a" c)))

(defun mdead (m) 
  (when (eq :eternal-rest (md-state m))
    (throw :mdead m)))

(defmacro def-c-output (slotname
                      (&optional (self-arg 'self) (new-varg 'new-value)
                                 (oldvarg 'old-value) (oldvargboundp 'old-value-boundp))
                      &body output-body)
  ;;;(trc nil "output body" outputbody)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',slotname :output-defined) t))
     ,(if (eql (last1 output-body) :test)
          (let ((temp1 (gensym))
                (loc-self (gensym)))
            `(defmethod c-output-slot-name #-(or clisp cormanlisp) progn ((slotname (eql ',slotname)) ,self-arg ,new-varg ,oldvarg ,oldvargboundp)
               (let ((,temp1 (bump-output-count ,slotname))
                     (,loc-self ,(if (listp self-arg)
                                     (car self-arg)
                                   self-arg)))
                 (when (and ,oldvargboundp ,oldvarg)
                   (format t "~&output ~d (~a ~a) old: ~a" ,temp1 ',slotname ,loc-self ,oldvarg))
                 (format t "~&output ~d (~a ~a) new: ~a" ,temp1 ',slotname ,loc-self ,new-varg))))
        `(defmethod c-output-slot-name
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

