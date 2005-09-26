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

(defmodel computer ()
  ((hear :cell :ephemeral :accessor hear :initform (c-in nil))
   (salutation :initarg :salutation :accessor salutation :initform "hello")
   (response :initform nil :initarg :response
	             :unchanged-if ‘string= :accessor response)))

(def-c-output response ()
  (when new-value
    (format t "~&hear: ~a~%respond: ~a" (hear self) new-value)))

(defun hello-world ()
  (cell-reset)
  (let ((system (make-instance 'computer
                 :response (c? (let ((r (case (hear self)
                                          (:knock-knock "who's there?")
                                          (:world (concatenate 'string
                                                     (salutation self)
                                                    ", "
                                                    (string (hear self))))
                                          ((nil) "<silence>"))))
                                 (if (string= r .cache)
                                     (format nil "i said, \"~a\"" r)
                                   r))))))
    (format t "~&to-be initialization complete")
    (setf (hear system) :knock-knock)
    (setf (hear system) :knock-knock)
    (setf (hear system) :world)
    (setf (salutation system) "hiya")
    (values)))

#+(or)
(hello-world)

#| output

hear: nil
respond: <silence>
hear: knock-knock
respond: who's there?
hear: knock-knock
respond: i said, "who's there?"
hear: world
respond: hello, world

|#

