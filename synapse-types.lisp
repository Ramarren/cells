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

(defmacro f-sensitivity (synapse-id (sensitivity &optional subtypename) &body body)
  `(call-f-sensitivity ,synapse-id ,sensitivity ,subtypename (lambda () ,@body)))

(defun call-f-sensitivity (synapse-id sensitivity subtypename body-fn)
  (with-synapse synapse-id (prior-fire-value)
    (let ((new-value (funcall body-fn)))
      (trc nil "f-sensitivity fire-p decides" prior-fire-value sensitivity)
      (let ((prop-code (if (or (xor prior-fire-value new-value)
                             (eko ("sens fire-p decides" new-value prior-fire-value sensitivity)
                                (delta-greater-or-equal
                                 (delta-abs (delta-diff new-value prior-fire-value subtypename)
                                   subtypename)
                                 (delta-abs sensitivity subtypename) 
                                 subtypename)))
                            :propagate
                          :no-propagate)))
        (values (if (eq prop-code :propagate)
                    (progn
                      (trc "sense prior fire value now" new-value)
                      (setf prior-fire-value new-value))
                  new-value) prop-code)))))

(defmacro f-delta (synapse-id (&key sensitivity (type 'number)) &body body)
  `(call-f-delta ,synapse-id ,sensitivity ',type (lambda () ,@body)))

(defun call-f-delta (synapse-id sensitivity type body-fn)
  (with-synapse synapse-id (last-relay-basis last-bound-p delta-cum)
       (let* ((new-basis (funcall body-fn))
              (threshold sensitivity)
              (tdelta (delta-diff new-basis
                         (if last-bound-p
                             last-relay-basis
                           (delta-identity new-basis type))
                         type)))
         (trc nil "tdelta, threshhold" tdelta threshold)
         (setf delta-cum tdelta)
         (let ((propagation-code
                (when threshold
                  (if (delta-exceeds tdelta threshold type)
                      (progn
                        (setf last-bound-p t)
                        (setf last-relay-basis new-basis)
                        :propagate)
                    :no-propagate))))
           (trc nil "f-delta returns values" delta-cum propagation-code)
           (values delta-cum propagation-code)))))

(defmacro f-plusp (key &rest body)
  `(with-synapse ,key (prior-fire-value) 
     (let ((new-basis (progn ,@body)))
       (values new-basis (if (xor prior-fire-value (plusp new-basis))
                             (progn
                               (setf prior-fire-value (plusp new-basis))
                               :propagate)
                           :no-propagate)))))

(defmacro f-zerop (key &rest body)
  `(with-synapse ,key (prior-fire-value) 
     (let ((new-basis (progn ,@body)))
       (values new-basis (if (xor prior-fire-value (zerop new-basis))
                             (progn
                               (setf prior-fire-value (zerop new-basis))
                               :propagate)
                           :no-propagate)))))



;;;(defun f-delta-list (&key (test #'true))
;;;  (with-synapse (prior-list)
;;;             :fire-p (lambda (syn new-list)
;;;                           (declare (ignorable syn))
;;;                           (or (find-if (lambda (new)
;;;                                            ;--- gaining one? ----
;;;                                            (and (not (member new prior-list))
;;;                                                 (funcall test new)))
;;;                                        new-list)
;;;                               (find-if (lambda (old)
;;;                                            ;--- losing one? ----
;;;                                            (not (member old new-list))) ;; all olds have passed test, so skip test here
;;;                                        prior-list)))
;;;             
;;;             :fire-value (lambda (syn new-list)
;;;                                (declare (ignorable syn))
;;;                                ;/// excess consing on long lists
;;;                                (setf prior-list (remove-if-not test new-list)))))

;;;(defun f-find-once (finder-fn)
;;;  (mk-synapse (bingo bingobound)
;;;
;;;             :fire-p (lambda (syn new-list)
;;;                            (declare (ignorable syn))
;;;                            (unless bingo ;; once found, yer done
;;;                              (setf bingobound t
;;;                                bingo (find-if finder-fn new-list))))
;;;
;;;             :fire-value (lambda (syn new-list)
;;;                                (declare (ignorable syn))
;;;                                (or bingo
;;;                                    (and (not bingobound) ;; don't bother if fire? already looked
;;;                                         (find-if finder-fn new-list))))))
                                
;;;(defun fdifferent ()
;;;  (mk-synapse (prior-object)
;;;    :fire-p (lambda (syn new-object)
;;;              (declare (ignorable syn))
;;;              (trc nil  "fDiff: prior,new" (not (eql new-object prior-object))
;;;                prior-object new-object)
;;;              (not (eql new-object prior-object)))
;;;    
;;;    :fire-value (lambda (syn new-object)
;;;                   (declare (ignorable syn))
;;;                   (unless (eql new-object prior-object)
;;;                     (setf prior-object new-object)))
;;;    ))


;;;(defun f-boolean (&optional (sensitivity 't))
;;;  (f-delta :sensitivity sensitivity :type 'boolean))
        

