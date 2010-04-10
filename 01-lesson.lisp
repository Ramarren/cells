(defmacro cells::ct-assert (form &rest stuff)
  `(progn
     (print `(attempting ,',form))
    (assert ,form () "Error with ~a >> ~a" ',form (list ,@stuff))))

(defpackage #:tu-selfinit (:use :cl :cells))

;;
;; We will keep making new packages so we can incrementally develop the
;; same class without newer versions stomping on earlier versions (by
;; being in the same package and effectively redefining earlier versions).
;;
(in-package #:tu-selfinit)

(defmodel rectangle ()
  ((len :initarg :len :accessor len
     :initform (c? (* 2 (width self))))
   (width :initarg :width :initform nil :accessor width))
  (:default-initargs
      :width (c? (/ (len self) 2))))

#+test
(cells::ct-assert (eql 21 (width (make-instance 'rectangle :len 42))))

;;; The first thing we see is that we are not creating something new, we are
;;; merely /extending/ CLOS. defmodel works like defclass in all ways, except for
;;; extensions to provide the behavior of Cells. We see both :initform
;;; and :default-initarg used to provide rules for a slot. We also see
;;; the initarg :len used to override the default initform.
;;;
;;; By extending defclass we (a) retain its expressiveness, and (b) produce
;;; something hopefully easier to learn by developers already familiar with CLOS.
;;;
;;; The first extension we see is that the len initform refers to the
;;; Smalltalk-like anaphoric variable self, to which will be bound 
;;; the rectangle instance being initialized. Normally an initform is evaluated 
;;; without being able to see the instance, and any initialization requiring
;;; that must be done in the class initializer.


