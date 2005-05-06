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

;;;(defmethod update-instance-for-redefined-class ((self model-object) added lost plist &key)
;;;  (declare (ignorable added lost plist))
;;;  (when (slot-boundp self '.md-state) (call-next-method)))

(defmethod occurence ((self model-object))
  ;
  ; whether multiply occuring or not, return index of self
  ; within list of likenamed siblings, perhaps mixed amongst others
  ; of diff names
  ;
  (let ((self-index -1))
     (dolist (kid (kids (fm-parent self)))
       (when (eql (md-name kid) (md-name self))
         (incf self-index)
         (when (eql self kid)
           (return-from occurence self-index))))))


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
  (if (md-untouchable self)
      (trc "not-to-be not quiescing untouchable" self)
    (md-quiesce self)))

(defmethod md-untouchable (self) ;; would be t for closed-stream under acl
  (declare (ignore self))
  nil)

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

(defun to-be (self)
  (trc nil "to-be> entry" self (md-state self))
  (when (eql :nascent (md-state self))
      (md-awaken self))
  self)

(defun make-be (class &rest initargs)
  (to-be (apply 'make-instance class initargs)))

