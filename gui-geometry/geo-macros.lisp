;; -*- mode: Lisp; Syntax: Common-Lisp; Package: gui-geometry; -*-
#|

Copyright (C) 2004 by Kenneth William Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package #:gui-geometry)

(defmacro ^offset-within (inner outer)
  (let ((offset-h (gensym)) (offset-v (gensym)) (from (gensym)))
     `(let ((,offset-h 0)
            (,offset-v 0))
         (do ((,from ,inner (fm-parent ,from)))
             ((or (null ,from)
                  (eql ,from ,outer))
              ;
              (mkv2 ,offset-h ,offset-v))
           
           (incf ,offset-h (px ,from))
           (incf ,offset-v (py ,from))))))

(defmacro ^ll-width (width)
     `(- (lr self) ,width))

(defmacro ^lr-width (width)
     `(+ (ll self) ,width))

(defmacro ^lt-height (height)
     `(- (lb self) ,height))

(defmacro ^lb-height (height)
     `(+ (lt self) ,height))

(defmacro ll-maintain-pL (pl)
     `(- ,pL (^px)))

(defmacro lr-maintain-pr (pr)
     `(- ,pr (^px)))

(defmacro ^fill-right (upperType &optional (padding 0))
  `(call-^fillRight self (upper self ,upperType) ,padding))

;recalc local top based on pT and offset
(defmacro lt-maintain-pT (pT)
     `(- ,pT (^py)))

;recalc local bottom based on pB and offset
(defmacro lb-maintain-pB (pB)
     `(- ,pB (^py)))

;------------------------------------
; recalc offset based on p and local 
;

(defmacro px-maintain-pL (pL)
  (let ((lL (gensym)))
     `(- ,pL (let ((,lL (^lL)))
               (c-assert ,lL () "^px-maintain-pL sees nil lL for ~a" self)
               ,lL))))

(defmacro px-maintain-pR (pR)
  `(- ,pR (^lR)))

(defmacro py-maintain-pT (pT)
  `(- ,pT (^lT)))

(defmacro py-maintain-pB (pB)
  `(- ,pB (^lB)))

(export! centered-h? centered-v? lb-maintain-pB)

(defmacro ^fill-down (upper-type &optional (padding 0))
  (let ((filled (gensym)))
    `(let ((,filled (upper self ,upper-type)))
       #+shhh (trc "^fillDown sees filledLR less offH"
                 (lb ,filled)
                 ,padding
                 (v2-v (offset-within self ,filled)))
       (- (lb ,filled)
          ,padding
          (v2-v (offset-within self ,filled))))))

(defmacro ^lbmax? (&optional (padding 0))
  `(c? (lb-maintain-pb (- (inset-lb .parent)
                            ,padding))))

(defmacro ^lrmax? (&optional (padding 0))
  `(c? (lr-maintain-pr (- (inset-lr .parent)
                            ,padding))))

; "...return the sib's pL [if ,alignment is :left] or pR, plus optional spacing"

(defmacro ^prior-sib-pr (self &optional (spacing 0) alignment)
   (let ((kid (gensym))
         (psib (gensym)))
      `(let* ((,kid ,self)
              (,psib (find-prior ,kid (kids (fm-parent ,kid)) :test (lambda (k) (not (collapsed k))))))
          (if ,psib
             (case ,alignment
               (:left (+ ,spacing (pl ,psib)))
               (otherwise (+ ,spacing (pr ,psib))))
             0))))

(defmacro ^px-stay-right-of (other &key (by '0))
   `(px-maintain-pl (+ (pr (fm-other ,other)) ,by)))

; in use; adjust offset to maintain pL based on ,justify
(defmacro ^px-self-centered (justify)
   `(px-maintain-pl
     (ecase ,justify
       (:left 0)
       (:center (floor (- (inset-width .parent) (l-width self)) 2))
       (:right (- (inset-lr .parent) (l-width self))))))

(defmacro ^fill-parent-right (&optional (inset 0))
  `(lr-maintain-pr (- (inset-lr .parent) ,inset)))

(defmacro ^fill-parent-down ()
  `(lb-maintain-pb (inset-lb .parent)))

(defmacro ^prior-sib-pt (self &optional (spacing 0))
   (let ((kid (gensym))
         (psib (gensym)))
      `(let* ((,kid ,self)
              (,psib (find-prior ,kid (kids (fm-parent ,kid)))))
          ;(trc "^priorSib-pb > kid, sib" ,kid ,pSib)
          (if ,psib
             (+ (- (abs ,spacing)) (pt ,psib))
             0))))



