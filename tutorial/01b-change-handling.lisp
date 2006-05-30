#| There is the fun part: automatic state management. Not only can a slot get its value from
a self-aware rule, but that value will stay current with other values as they change.

But often changes to a value must be reflected outside the automatic dataflow model. See next.

|#

(defpackage #:tu-change-handling (:use :cl :cells))
(in-package #:tu-change-handling)

(defmodel rectangle ()
  ((len :initarg :len :accessor len
     :initform (c? (* 2 (width self))))
   (width :initarg :width :initform nil :accessor width))
  (:default-initargs
      :width (c? (/ (len self) 2))))

(defvar *gui-told*)

(defobserver len ((self rectangle) new-value old-value old-value-bound-p)
  ;; Where rectangle is a GUI element, we need to tell the GUI framework
  ;; to update this area of the screen
  (setf *gui-told* t)
  (print (list "tell GUI about" self new-value old-value old-value-bound-p)))

#+test
(let* ((*gui-told* nil)
       (r (make-instance 'rectangle :len (c-in 42))))
  (cells::ct-assert *gui-told*)
  (setf *gui-told* nil)
  (cells::ct-assert (eql 21 (width r)))

  (cells::ct-assert (= 1000 (setf (len r) 1000)))
  (cells::ct-assert *gui-told*)
  (cells::ct-assert (eql 500 (width r))))

