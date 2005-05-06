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

(defmacro f-sensitivity ((sensitivity &optional subtypename) &body body)
  `(with-synapse ((prior-fire-value)
                  :fire-p (lambda (syn new-value)
                            (declare (ignorable syn))
                            (trc nil "f-sensitivity fire-p decides" prior-fire-value ,sensitivity)
                            (or (xor prior-fire-value new-value)
                              (eko (nil "fire-p decides" new-value prior-fire-value ,sensitivity)
                                (delta-greater-or-equal
                                 (delta-abs (delta-diff new-value prior-fire-value ,subtypename)
                                   ,subtypename)
                                 (delta-abs ,sensitivity ,subtypename) 
                                 ,subtypename))))
                  
                  :fire-value (lambda (syn new-value)
                                (declare (ignorable syn))
                                (eko (nil "fsensitivity relays")
                                  (setf prior-fire-value new-value))))
     ,@body))

(defmacro f-delta ((&key sensitivity (type 'number)) &body body)
  (let ((threshold (gensym)) (tdelta (gensym)))
    `(with-synapse ((last-relay-basis last-bound-p delta-cum)
                    :fire-p (lambda (syn new-basis)
                              (declare (ignorable syn))
                              (let ((,threshold ,sensitivity)
                                    (,tdelta (delta-diff new-basis
                                               (if last-bound-p
                                                   last-relay-basis
                                                 (delta-identity new-basis ',type))
                                               ',type)))
                                (trc "tdelta, threshhold" ,tdelta ,threshold)
                                (setf delta-cum ,tdelta)
                                (eko ("delta fire-p")
                                  (or (null ,threshold)
                                    (delta-exceeds ,tdelta ,threshold ',type)))))
                    
                    :fire-value (lambda (syn new-basis)
                                  (declare (ignorable syn))
                                  (trc "f-delta fire-value gets" delta-cum new-basis syn)
                                  (trc "fdelta > new lastrelay" syn last-relay-basis)
                                  (setf last-bound-p t)
                                  (setf last-relay-basis new-basis)
                                  delta-cum))
       ,@body)))

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
                                
;;;(defun f-plusp ()
;;;  (mk-synapse (prior-fire-value)
;;;    :fire-p (lambda (syn new-basis)
;;;              (declare (ignorable syn))
;;;              (eko (nil "fPlusp fire-p decides" prior-fire-value sensitivity)
;;;                (xor prior-fire-value (plusp new-basis))))
;;;    
;;;    :fire-value (lambda (syn new-basis)
;;;                   (declare (ignorable syn))
;;;                   (eko (nil "fPlusp relays")
;;;                     (setf prior-fire-value (plusp new-basis))) ;; no modulation of value, but do record for next time
;;;                   )))

;;;(defun f-zerop ()
;;;  (mk-synapse (prior-fire-value)
;;;    :fire-p (lambda (syn new-basis)
;;;              (declare (ignorable syn))
;;;              (eko (nil "fZerop fire-p decides")
;;;                (xor prior-fire-value (zerop new-basis))))
;;;    
;;;    :fire-value (lambda (syn new-basis)
;;;                   (declare (ignorable syn))
;;;                   (eko (nil "fZerop relays")
;;;                     (setf prior-fire-value (zerop new-basis)))
;;;                   )))

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
        

