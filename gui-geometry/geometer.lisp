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

(export! geo-kid-sized)
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

(export! offset-within inset-lb)
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
  (if (eq outer inner)
     outer-v2
     (v2-xlate outer (fm-parent inner)
               (v2-subtract outer-v2
                            (mkv2 (px inner) (py inner))))))

(defun v2-xlate-out (inner outer inner-v2)
  (if (eq outer inner)
      inner-v2
    (v2-xlate (fm-parent inner) outer
      (v2-add inner-v2
        (mkv2 (px inner) (py inner))))))

(defun v2-xlate-between (from-v2 from to)
  (cond
   ((fm-includes from to)(v2-xlate from to from-v2))
   ((fm-includes to from)(v2-xlate-out from to from-v2))
   (t (break "time to extend v2-xlate-between"))))

(export! h-xlate v-xlate v2-xlate-between)

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

(defun inset-lt (self)
  (downs (lt self) (outset self)))

(defun inset-height (self)
   (- (l-height self) (outset self) (outset self)))

;---------------------------------

;----------------------------------

(export! geo-kid-wrap inset-lt)

(defun geo-kid-wrap (self bound)
  (funcall (ecase bound ((pl pb) '-)((pr pt) '+))
    (funcall (ecase bound
               ((pl pb) 'fm-min-kid)
               ((pr pt) 'fm-max-kid)) self bound)
    (outset self)))

; in use; same idea for pT
(defun py-self-centered (self justify)
  (py-maintain-pt
   (ecase justify
     (:top  0)
     (:center (floor (- (inset-height .parent) (l-height self)) -2))
     (:bottom (downs (- (inset-height .parent) (l-height self)))))))

