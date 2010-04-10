;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
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

;;;
;;;(defstrudel computer
;;;  (happen :cell :ephemeral :initform (c-in nil))
;;;   (location :cell t
;;;             :initform (c? (case (^happen)
;;;                              (:leave :away)
;;;                              (:arrive :at-home)
;;;                              (t (c-value c))))
;;;             :accessor location)
;;;   (response :cell :ephemeral :initform nil :initarg :response :accessor response)))

(def-c-output response((self computer) new-response old-response)
  (when new-response
    (format t "~&computer: ~a" new-response)))

(def-c-output happen((self computer))
  (when new-value
    (format t "~&happen: ~a" new-value)))

(defun hello-world-q ()
  (let ((dell (make-instance 'computer
                 :response (c? (bwhen (h (happen self))
                                 (if (eql (^location) :at-home)
                                     (case h
                                       (:knock-knock "who's there?")
                                       (:world "hello, world."))
                                   "<silence>"))))))
    (dotimes (n 2)
      (setf (happen dell) :knock-knock))
    (setf (happen dell) :arrive)
    (setf (happen dell) :knock-knock)
    (setf (happen dell) :world)
    (values)))

#+(or)
(hello-world)

#+(or)
(traceo sm-echo)


#| output

happen: knock-knock
computer: <silence>
happen: knock-knock
computer: <silence>
happen: arrive
happen: knock-knock
computer: who's there?
happen: world
computer: hello, world.

|#
