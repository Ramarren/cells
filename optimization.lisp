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

;---------- optimizing away cells whose dependents all turn out to be constant ----------------
;

(defun c-optimize-away?! (c)
  (declare (ignorable c))

  (typecase c
    (c-dependent
     (if (and *c-optimizep*
           (not (c-optimized-away-p c)) ;; c-streams come this way repeatedly even if optimized away
           (c-validp c)
           (not (c-synaptic c)) ;; no slot to cache invariant result, so they have to stay around)
           (null (cd-useds c)))
         
         (progn
           (trc nil "optimizing away" c (c-state c))
           (count-it :c-optimized)
           
           (setf (c-state c) :optimized-away)

           (let ((entry (rassoc c (cells (c-model c))))) ; move from cells to cells-flushed
             (c-assert entry)
             (setf (cells (c-model c)) (delete entry (cells (c-model c))))
             (push entry (cells-flushed (c-model c))))
           
           (dolist (user (c-users c))
             (setf (cd-useds user) (delete c (cd-useds user)))
             (trc nil "checking opti2" c :user> user)
             (when (c-optimize-away?! user)
               (trc "Wow!!! optimizing chain reaction, first:" c :then user)))
           t)
       
       (progn
         (trc nil "not optimizing away" *c-optimizep* (car (cd-useds c)) (c-validp c))
         #+no (dolist (used (cd-useds c))
                (c-assert (member c (c-users used)))
                ;;; (trc nil "found as user of" used)
                )
         ;  (count-it :c-not-optimize)
         ;  (count-it (intern-keyword "noopti-" #+nah (c-model c) "-" (symbol-name (c-slot-name c))))
         )))))
