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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(family-values family-values-sorted
            sort-index sort-direction sort-predicate sort-key
            ^sort-index ^sort-direction ^sort-predicate ^sort-key)))

(defmodel family-values (family)
  (
   (kv-collector :initarg :kv-collector
     :initform #'identity
     :reader kv-collector)
   
   (kid-values :initform (c? (when (kv-collector self)
                               (funcall (kv-collector self) (^md-value))))
     :accessor kid-values
     :initarg :kid-values)
   
   (kv-key :initform #'identity
     :initarg :kv-key
     :reader kv-key)
   
   (kv-key-test :initform #'equal
     :initarg :kv-key-test
     :reader kv-key-test)
   
   (kid-factory :initform #'identity
     :initarg :kid-factory
     :reader kid-factory)
   
   (.kids :initform (c? (c-assert (listp (kid-values self)))
                      (let ((new-kids (mapcan (lambda (kid-value)
                                                (list (or (find kid-value .cache
                                                            :key (kv-key self)
                                                            :test (kv-key-test self))
                                                        (trc nil "family-values forced to make new kid" 
                                                          self .cache kid-value)
                                                        (funcall (kid-factory self) self kid-value))))
                                        (^kid-values))))
                        (nconc (mapcan (lambda (old-kid)
                                         (unless (find old-kid new-kids)
                                           (when (fv-kid-keep self old-kid)
                                             (list old-kid))))
                                 .cache)
                          new-kids)))
     :accessor kids
     :initarg :kids)))

(defmethod fv-kid-keep (family old-kid)
  (declare (ignorable family old-kid))
  nil)

(defmodel family-values-sorted (family-values)
  ((sorted-kids :initarg :sorted-kids :accessor sorted-kids
     :initform nil)
   (sort-map :initform (c-in nil) :initarg :sort-map :accessor sort-map)
   (.kids :initform (c? (c-assert (listp (kid-values self)))
                 (mapsort (^sort-map)
                   (the-kids
                    (mapcar (lambda (kid-value)
                              (trc "making kid" kid-value)
                              (or (find kid-value .cache :key (kv-key self) :test (kv-key-test self))
                                (trc nil "family-values forced to make new kid" self .cache kid-value)
                                (funcall (kid-factory self) self kid-value)))
                      (^kid-values)))))
     :accessor kids
     :initarg :kids)))

(defun mapsort (map data)
  ;;(trc "mapsort map" map)
  (if map
      (stable-sort data #'< :key (lambda (datum) (or (position datum map)
                                                       ;(trc "mapsort datum not in map" datum)
                                                       (1+ (length data)))))
    data))

(def-c-output sorted-kids ()
  (setf (sort-map self) new-value)) ;; cellular trick to avoid cyclicity

