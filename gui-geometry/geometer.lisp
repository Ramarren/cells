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

(eval-now!
  (export '(outset ^outset mkv2 g-offset g-offset-h g-offset-v collapsed ^collapsed inset ^inset)))

(defmd geometer ()
  px py ll lt lr lb
  collapsed
  (inset (mkv2 0 0) :unchanged-if 'v2=)
  (outset 0)
  (w-box (mkr 0 0 0 0) :cell nil :accessor w-box
    :documentation "bbox in window coordinate system"))

(defmethod collapsed (other)
  (declare (ignore other))
  nil)

;;-------- Zero-zero Top Left ----------------------------
;;
(defmodel geo-zero-tl (family) 
   ()
   (:default-initargs
    :ll (c? (- (outset self))) 
    :lt (c? (+ (outset self))) 
    :lr (c? (geo-kid-wrap self 'pr)) 
    :lb (c? (geo-kid-wrap self 'pb))
    :kid-slots (def-kid-slots
                   (mk-kid-slot (px :if-missing t)
                     (c? (px-maintain-pl 0)))
                   (mk-kid-slot (py :if-missing t)
                     (c? (py-maintain-pt 0))))))

(defmodel geo-kid-sized (family) 
    ()
    (:default-initargs
        :ll (c? (geo-kid-wrap self 'pl))
      :lt (c? (geo-kid-wrap self 'pt))
     :lr (c? (geo-kid-wrap self 'pr))
      :lb (c? (geo-kid-wrap self 'pb))))

(defun l-box (geo)
  (count-it :l-box)
   (mkr (ll geo) (lt geo) (lr geo) (lb geo)))

;---------- gOffset -------------------

(export! offset-within)
;
(defun offset-within (inner outer &optional dbg)
  (declare (ignorable dbg))
  (trc nil "offset-within inner outer" inner outer)
  (do (
       (offset-h 0 (progn
                    (trc nil "offset-within delta-h, from" from (px from))
                    (incf offset-h (px from))))
       (offset-v 0 (incf offset-v (py from)))
       (from inner (fm-parent from)))
      ((or (null from)
           (null outer)
           (eql from outer)) (eko (nil "offset-within returns")
                                  (mkv2 offset-h offset-v)))))

(defun offset-within2 (inner outer)
  (do (
       (offset-h 0 (incf offset-h (px from)))
       (offset-v 0 (incf offset-v (py from)))
       (from inner (fm-parent from)))
      ((or (null from)
           (null outer)
           (eql from outer)) (mkv2 offset-h offset-v))
    ;(trc "inner outer" inner outer)
    ))

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

;----------- OfKids -----------------------
;

(defun v2-in-subframe (super h v sub)
  (if (eql super sub) ;; bingo
      (values h v)
    (dolist (kid (kids super))
      (multiple-value-bind (subh sub-v)
          (v2-in-subframe kid h v sub)
        (when subh
          (return-from v2-in-subframe (values (- subh (px kid))
                                              (- sub-v (py kid)))))))))
(defun mk-gr (geo)
   (c-assert geo)
  (count-it :mk-gr)
  (let ((g-offset (g-offset geo))) ;; /// wastes a v2
    (nr-offset (mkr (ll geo) (lt geo) (lr geo) (lb geo)) (v2-h g-offset) (v2-v g-offset))))

;sum pXYs up the family tree    ;gave an odd result for cursor display....

(defun v2-xlate (outer inner outer-v2)
  (if (eql outer inner)
     outer-v2
     (v2-xlate outer (fm-parent inner)
               (v2-subtract outer-v2
                            (mkv2 (px inner) (py inner))))))

(defun h-xlate (outer inner outer-h)
  (if (eql outer inner)
     outer-h
     (h-xlate outer (fm-parent inner)
               (- outer-h (px inner)))))

(defun v-xlate (outer inner outer-v)
  (if (eql outer inner)
     outer-v
     (v-xlate outer (fm-parent inner)
               (- outer-v (py inner)))))

(defmethod g-offset (self &optional (accum-h 0) (accum-v 0) within)
  (declare (ignorable self within))
  (mkv2 accum-h accum-v))

(defun g-offset-h (geo)
   (v2-h (g-offset geo)))

(defun g-offset-v (geo)
     (v2-v (g-offset geo)))

(defun g-box (geo)
  (count-it :g-box)
  (if (c-stopped)
      (trc "gbox sees stop" geo)
    (progn
      (c-assert geo)
      (let* ((g-offset (g-offset geo))
             (oh (v2-h g-offset)))
        (c-assert (typep g-offset 'v2))
        (c-assert (numberp oh))
        (c-assert (numberp (lr geo)))
        (let ((r (nr-offset
                  (nr-make (w-box geo) (ll geo) (lt geo) (lr geo) (lb geo))
                   oh (v2-v g-offset))))
          (c-assert (numberp (r-left r)))
          (c-assert (numberp (r-top r)))
          (c-assert (numberp (r-right r)))
          (c-assert (numberp (r-bottom r)))
          r)))))

;____________________________________________

(defun pl (self) (+ (px self) (ll self)))
(defun pr (self)
  (c-assert (px self))
  (c-assert (lr self))
  (+ (px self) (lr self)))
(defun pt (self) (+ (py self) (lt self)))
(defun pb (self)
  (c-assert (lb self))
  (c-assert (py self))
  (+ (py self) (lb self)))

(defun pxy (self)
  (mkv2 (px self) (py self)))

;--------------------------------------------------------


(defun l-width (i)
  (c-assert (lr i))
  (c-assert (ll i))
  (- (lr i) (ll i)))

(defun l-height (i)
   (abs (- (lb i) (lt i))))

;;-----------------------------------------------

(defun inset-width (self)
   (- (l-width self) (outset self) (outset self)))

(defun inset-lr (self)
   (- (lr self) (outset self)))

(defun inset-lb (self)
   (+ (lb self) (outset self)))

(defun inset-height (self)
   (- (l-height self) (outset self) (outset self)))

;---------------------------------

(defmacro ^ll-width (width)
     `(- (lr self) ,width))

(defmacro ^lr-width (width)
     `(+ (ll self) ,width))

(defmacro ^lt-height (height)
     `(- (lb self) ,height))

(defmacro ^lb-height (height)
     `(+ (lt self) ,height))

;----------------------------------

(export! geo-kid-wrap)

(defun geo-kid-wrap (self bound)
  (funcall (ecase bound ((pl pb) '-)((pr pt) '+))
    (funcall (ecase bound
               ((pl pb) 'fm-min-kid)
               ((pr pt) 'fm-max-kid)) self bound)
    (outset self)))

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

;--------------
;recalc offset based on p and local 
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

(defmacro centered-h? ()
  `(c? (px-maintain-pl (round (- (l-width .parent) (l-width self)) 2))))

(defmacro ^centered-v? ()
  `(c? (py-maintain-pt (round (- (l-height .parent) (l-height self)) 2))))

(defmacro ^fill-down (upper-type &optional (padding 0))
  (let ((filled (gensym)))
    `(let ((,filled (upper self ,upper-type)))
       #+qt (trc "^fillDown sees filledLR less offH"
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

(defun ^prior-sib-pb (self &optional (spacing 0))
  (bif (psib (find-prior self (kids .parent)
               :test (lambda (sib)
                       (not (collapsed sib)))))
    (eko (nil "^prior-sib-pb spc pb-psib -lt" (- (abs spacing)) (pb psib) (- (^lt)))
      (+ (- (abs spacing)) ;; force spacing to minus(= down for OpenGL)
        (pb psib)))   
      0))

(defmacro ^prior-sib-pt (self &optional (spacing 0))
   (let ((kid (gensym))
         (psib (gensym)))
      `(let* ((,kid ,self)
              (,psib (find-prior ,kid (kids (fm-parent ,kid)))))
          ;(trc "^priorSib-pb > kid, sib" ,kid ,pSib)
          (if ,psib
             (+ (- (abs ,spacing)) (pt ,psib))
             0))))

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

; in use; same idea for pT
(defun py-self-centered (self justify)
  (py-maintain-pt
   (ecase justify
     (:top  0)
     (:center (floor (- (inset-height .parent) (l-height self)) -2))
     (:bottom (downs (- (inset-height .parent) (l-height self)))))))

(defmacro ^fill-parent-right (&optional (inset 0))
  `(lr-maintain-pr (- (inset-lr .parent) ,inset)))

(defmacro ^fill-parent-down ()
  `(lb-maintain-pb (inset-lb .parent)))

