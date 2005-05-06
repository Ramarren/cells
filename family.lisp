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

(eval-when (:compile-toplevel :execute :load-toplevel)
  (export '(model md-value family kids kid1 perishable)))

(defmodel model ()
  ((.md-name :cell nil :initform nil :initarg :md-name :accessor md-name)
   (.fm-parent :cell nil :initform nil :initarg :fm-parent :accessor fm-parent)
   (.md-value :initform nil :accessor md-value :initarg :md-value)))

(defmethod fm-parent (other)
  (declare (ignore other))
  nil)

(defmethod print-object ((self model) s)
  (format s "~a" (or (md-name self) (type-of self))))

(define-symbol-macro .parent (fm-parent self))

(defmethod md-initialize :around ((self model))
  (when (slot-boundp self '.md-name)
    (unless (md-name self)
      (setf (md-name self) (c-class-name (class-of self)))))
      
  (when (fm-parent self)
    (md-adopt (fm-parent self) self))

  (call-next-method))

(defmodel perishable ()
  ((expiration :initform nil :accessor expiration :initarg :expiration)))

(def-c-output expiration ()
  (when new-value
    (not-to-be self)))

(defmodel family (model)
  ((.kid-slots :cell nil
         :initform nil
         :accessor kid-slots
         :initarg :kid-slots)
   (.kids :initform (c-in nil) ;; most useful
         :accessor kids
         :initarg :kids)
   ))

(defmacro the-kids (&rest kids)
  `(packed-flat! ,@(mapcar (lambda (kid)
                             (typecase kid
                               (keyword  `(make-instance ',(intern$ (symbol-name kid))))
                               (t `,kid)))
                     kids)))

(defmacro the-kids-2 (&rest kids)
  `(packed-flat! ,@(mapcar (lambda (kid)
                             (typecase kid
                               (keyword  `(make-instance ',(intern$ (symbol-name kid))))
                               (t `,kid)))
                           kids)))

(defun kid1 (self) (car (kids self)))
(defun last-kid (self) (last1 (kids self)))

;; /// redundancy in following

(defmacro psib (&optional (self-form 'self))
  (let ((self (gensym)))
    `(bwhen (,self ,self-form)
        (find-prior ,self (kids (fm-parent ,self))))))

(defmacro nsib (&optional (self-form 'self))
  (let ((self (gensym)))
    `(bwhen (,self ,self-form)
        (cadr (member ,self (kids (fm-parent ,self)))))))

(defmacro ^prior-sib (self)
   (let ((kid (gensym)))
      `(let* ((,kid ,self))
          (find-prior ,kid (kids (fm-parent ,kid))))))

(defmacro ^first-kid-p (self)
   (let ((kid (gensym)))
      `(let ((,kid ,self))
          (eql ,kid (car (kids (fm-parent ,kid)))))))

(defmacro ^last-kid-p (self)
   (let ((kid (gensym)))
      `(let ((,kid ,self))
          (null (cdr (member ,kid (kids (fm-parent ,kid))))))))

(defun md-adopt (fm-parent self)
  (c-assert self)
  (c-assert fm-parent)
  (c-assert (typep fm-parent 'family))
  

  (trc nil "md-adopt >" :kid self (adopt-ct self) :by fm-parent)
  
  (let ((curr-parent (fm-parent self))
        (selftype (type-of self)))
    (c-assert (or (null curr-parent)
                (eql fm-parent curr-parent)))
    (when (plusp (adopt-ct self))
      (c-break "2nd adopt ~a, by ~a" self fm-parent))
    (unless (plusp (adopt-ct self))
      (incf (adopt-ct self))
      (setf (fm-parent self) fm-parent)

      (bwhen (kid-slots-fn (kid-slots (fm-parent self)))
        (dolist (ks-def (funcall kid-slots-fn self) self)
          (let ((slot-name (ks-name ks-def)))
            (trc nil "got ksdef " slot-name)
            (when (md-slot-cell-type selftype slot-name)
              (trc nil "got cell type " slot-name)
              (when (or (not (ks-if-missing ks-def))
                      
                      (and (null (c-slot-value self slot-name))
                        (null (md-slot-cell self slot-name))))
                (trc nil "ks missing ok " slot-name)
                (multiple-value-bind (c-or-value suppressp)
                    (funcall (ks-rule ks-def) self)
                  (unless suppressp
                    (trc nil "c-install " slot-name c-or-value)
                    (c-install self slot-name c-or-value))))))))

      ; new for 12/02...
      (md-adopt-kids self)))
  self)

(defmethod md-adopt-kids (self) (declare (ignorable self)))
(defmethod md-adopt-kids ((self family))
  (when (slot-boundp self '.kids)
    (dolist (k (slot-value self '.kids))
      (unless (fm-parent k)
        (md-adopt self k)))))




(defmethod c-slot-value ((self model-object) slot)
  (slot-value self slot))

(defun md-kids-change (self new-kids old-kids usage)
  (c-assert (listp new-kids))
  (c-assert (listp old-kids))
  (c-assert (not (member nil old-kids)))
  (c-assert (not (member nil new-kids)))

  (trc nil "md-kids-change > entry" usage new-kids old-kids)
  #+not (when (and (trcp self)
          (eql usage :mdslotvalueassume))
    (c-break "how here? ~a" self))

  (dolist (k old-kids)
     (unless (member k new-kids)
       (trc nil "kids change nailing lost kid" k)
       (not-to-be k)
       (setf (fm-parent k) nil) ;; 020302kt unnecessary? anyway, after not-to-be since that might require fm-parent
       ))

  (when (find-if 'zerop new-kids :key 'adopt-ct)
    (dolist (k new-kids)
      (trc nil "kids change sees new kid" self k)
      (unless (member k old-kids)       
        (if (eql :nascent (md-state k))
            (progn
              #+dfdbg (trc k "adopting par,k:" self k)
              (md-adopt self k))
          (unless (eql self (fm-parent k))
            ;; 230126 recent changes to kids handling leads to dup kids-change calls
            (trc "feature not yet implemented: adopting previously adopted: parent, kid" self (fm-parent k) (md-state k) (type-of k))
            (trc "old" old-kids)
            (trc "new" new-kids)
            (break "bad state extant nkid ~a ~a ~a" usage k (md-state k))
            ))))))

(def-c-output .kids ((self family))
  (dolist (k new-value)
    (to-be k)))

(defmethod kids ((other model-object))  nil)

(defmethod not-to-be :before ((fm family))
  (unless (md-untouchable fm)
    ;; use backdoor so if kids not yet ruled into
    ;; existence they won't be now just to not-to-be them
    (let ((sv-kids (slot-value fm '.kids)))
      (when (listp sv-kids)
        (dolist ( kid sv-kids)
          (not-to-be kid))))))


;------------------  kid slotting ----------------------------
;
(defstruct (kid-slotdef
           (:conc-name nil))
  ks-name
  ks-rule
  (ks-if-missing t))

(defmacro mk-kid-slot ((ks-name &key if-missing) ks-rule)
   `(make-kid-slotdef
     :ks-name ',ks-name
     :ks-rule (lambda (self)
                 (declare (ignorable self))
                 ,ks-rule)
     :ks-if-missing ,if-missing))

(defmacro def-kid-slots (&rest slot-defs)
  `(lambda (self)
     (declare (ignorable self))
     (list ,@slot-defs)))

(defmethod md-name (symbol)
     symbol)

(defmethod md-name ((nada null))
  (unless (c-stopped)
    (c-stop :md-name-on-null)
    (break "md-name called on nil")))

