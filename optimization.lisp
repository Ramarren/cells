;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

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
           ;; chop (every (lambda (lbl-syn) (null (cd-useds (cdr lbl-syn)))) (cd-synapses c))
           (not (c-inputp c))
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
             (c-optimize-away?! user) ;; rare but it happens when rule says (or .cache ...)
             )
           t)
       
       (progn
         (trc nil "not optimizing away" *c-optimizep* (car (cd-useds c)) (c-validp c))
         ;  (count-it :c-not-optimize)
         ;  (count-it (intern-keyword "noopti-" #+nah (c-model c) "-" (symbol-name (c-slot-name c))))
         )))))
