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

(in-package :gui-geometry)

(eval-now!
  (export '(v2 mkv2 v2=)))
;-----------------------------

(defstruct v2 
  (h 0 )  ;; horizontal coordinate
  (v 0 )  ;; vertical coordinate
  )

(defmethod print-object ((self v2) s)
  (format s "~a|~a" (v2-h self)(v2-v self)))

(defun mkv2 (h v) (make-v2 :h h :v v))

(defun v2= (a b)
  (and a b
    (= (v2-h a)(v2-h b))
    (= (v2-v a)(v2-v b))))

(defun v2-add (p1 p2-or-x &optional y-or-p2-or-x-is-p2)
  (if y-or-p2-or-x-is-p2
      (make-v2 :h (+ (v2-h p1) p2-or-x)
        :v (+ (v2-v p1) y-or-p2-or-x-is-p2))
      (make-v2 :h (+ (v2-h p1) (v2-h p2-or-x))
        :v (+ (v2-v p1) (v2-v p2-or-x)))))

(defun v2-subtract (p1 p2-or-x &optional y-or-p2-or-x-is-p2)
  (if y-or-p2-or-x-is-p2
      (make-v2 :h (- (v2-h p1) p2-or-x)
        :v (- (v2-v p1) y-or-p2-or-x-is-p2))
      (make-v2 :h (- (v2-h p1) (v2-h p2-or-x))
        :v (- (v2-v p1) (v2-v p2-or-x)))))

(defun v2-nmove (p1 x &optional y)
  (if y
      (progn
        (incf (v2-h p1) x)
        (incf (v2-v p1) y))
    (v2-nmove p1 (v2-h x)(v2-v x)))
  p1)

(defun v2-in-rect (v2 r)
  (mkv2 (min (r-right r) (max (r-left r) (v2-h v2)))
    (min (r-top r) (max (r-bottom r) (v2-v v2)))))

(defun v2-in-rect-ratio (v2 r)
  (assert (<= (r-left r) (v2-h v2) (r-right r)))
  (assert (<= (r-bottom r) (v2-v v2) (r-top r)))
  (mkv2 (div-safe (- (v2-h v2) (r-left r)) (r-width r))
    (div-safe (- (v2-v v2) (r-bottom r)) (r-height r))))

(defun div-safe (n d &optional (zero-div-return-value 1))
  (if (zerop d) zero-div-return-value (/ n d)))

(defmethod c-value-incf (c (base v2) (delta number))
  (declare (ignore c))
  (mkv2 (+ (v2-h base) delta)
    (+ (v2-v base) delta)))

(defmethod c-value-incf (c (base v2) (delta v2))
  (declare (ignore c))
  (v2-add base delta))

; synapse support
;
(defmethod delta-diff ((new v2) (old v2) (subtypename (eql 'v2)))
  (v2-subtract new old))

(defmethod delta-identity ((dispatcher number) (subtypename (eql 'v2)))
  (mkv2 0 0))

(defun long-v2 (long-hv)
    (c-assert (numberp long-hv))
    (multiple-value-bind (fv fh)
           (floor long-hv 65536)
          (mkv2 fh fv)))

(defun long-x (long-hv)
    (c-assert (numberp long-hv))
    (mod long-hv 65536))

(defun long-y (long-hv)
    (c-assert (numberp long-hv))
    (floor long-hv 65536))

(defun v2-long (v2)
  (c-assert (typep v2 'v2))
  (xy-long (v2-h v2) (v2-v v2)))

(defun xy-long (x y)
  (+  (* 65536 y) x))

(defun v2-to-vector (v2)
  (vector (v2-h v2) (v2-v v2)))

(defun v2-negative (v2)
   (c-assert (typep v2 'v2))
   (mkv2 (- (v2-h v2)) (- (v2-v v2))))

(defun vector-v2 (vc) (mkv2 (elt vc 0) (elt vc 1)))

(defmethod delta-exceeds ((d1 v2) (d2 v2) (subtypename (eql 'v2)))
     (c-assert (and (typep d1 'v2) (typep d2 'v2)))
     (> (v2-distance-to d1) (v2-distance-to d2)))

(defun v2-distance (from to)
     (sqrt (+ (expt (v2-dv from to) 2)
                 (expt (v2-dh from to) 2))))

(defun v2-area (v2)
  "Treat point as length & width and calc area"
  (abs (* (v2-h v2)(v2-v v2))))

(defun v2-dh (p1 p2)
     (- (v2-h p2) (v2-h p1)))

(defun v2-dv (p1 p2)
     (- (v2-v p2) (v2-v p1)))

(defun v2-angle-between (from to)
  (atan (v2-dv from to) (v2-dh from to)))

(defun v2-distance-to (to)
  (sqrt (+ (expt (v2-h to) 2)
           (expt (v2-v to) 2))))
;-------------------------------------------------

(export! rect)
(defstruct (rect (:conc-name r-))
  (left 0 )
  (top 0 )
  (right 0 )
  (bottom 0 ))

(defmethod print-object ((self rect) s)
  (format s "(rect (~a,~a) (~a,~a))" (r-left self)(r-top self)(r-right self)(r-bottom self)))

(defun r-top-left (r)
  (mkv2 (r-left r) (r-top r)))

(export! r-center)

(defun r-center (r)
  (mkv2 (/ (+ (r-left r)(r-right r)) 2)
    (/ (+ (r-top r)(r-bottom r)) 2)))

(defun r-bottom-right (r)
  (mkv2 (r-bottom r) (r-right r)))

(defun mkr (left top right bottom)
   (count-it :mkrect)
   (make-rect :left left :top top :right right :bottom bottom))

(defun nr-make (r left top right bottom)
   (setf (r-left r) left (r-top r) top (r-right r) right (r-bottom r) bottom)
  r)

(defmacro with-r-bounds ((lv tv rv bv) r-form &body body)
  (let ((r (gensym)))
    `(let* ((,r ,r-form)
            (,lv (r-left ,r))
            (,tv (r-top ,r))
            (,rv (r-right ,r))
            (,bv (r-bottom ,r)))
       ,@body)))

(defun ncopy-rect (old &optional new)
  (if new
      (progn
        (setf (r-left new)(r-left old)
          (r-top new)(r-top old)
          (r-right new)(r-right old)
          (r-bottom new)(r-bottom old))
        new)
    (copy-rect old)))

(defun r-inset (r in &optional (destr (mkr 0 0 0 0)))
  (nr-make destr
    (+ (r-left r) in) 
    (+ (r-top r) (downs in))
    (- (r-right r) in)
    (+ (r-bottom r) (ups in))))

(defun nr-make-from-corners (r tl br)
   (nr-make r (v2-h tl)(v2-v tl)(v2-h br)(v2-v br)))

(defun nr-copy (r copied-r)
   (setf (r-left r) (r-left copied-r)
     (r-top r) (r-top copied-r)
     (r-right r) (r-right copied-r)
     (r-bottom r) (r-bottom copied-r))
  r)

(defun r-contains (r v2)
  (and (<= (r-left r)(v2-h v2)(r-right r))
    (<= (r-top r)(v2-v v2)(r-bottom r))))

(defun nr-intersect (r sr)
  (let ((r-min-v (min (r-top r) (r-bottom r)))
        (r-max-v (max (r-top r) (r-bottom r)))
        (r-min-h (min (r-left r) (r-right r)))
        (r-max-h (max (r-left r) (r-right r)))
        ;
        (sr-min-v (min (r-top sr) (r-bottom sr)))
        (sr-max-v (max (r-top sr) (r-bottom sr)))
        (sr-min-h (min (r-left sr) (r-right sr)))
        (sr-max-h (max (r-left sr) (r-right sr)))
        )
   (let ((min-v (max r-min-v sr-min-v))
         (max-v (min r-max-v sr-max-v))
         (min-h (max r-min-h sr-min-h))
         (max-h (min r-max-h sr-max-h)))
     (when (or (>= min-v max-v)(>= min-h max-h))
       (setf min-h 0 min-v 0 max-h 0 max-v 0))
     (nr-make r min-h min-v max-h max-v))))

(defun nr-union (r sr) ;; unlike other code, this is assuming opengl's up==plus, and proper rectangles
  (nr-make r (min (r-left r) (r-left sr))
    (max (r-top r) (r-top sr))
    (max (r-right r) (r-right sr))
    (min (r-bottom r) (r-bottom sr))))

(defun nr-move-to (r h v)
   (setf (r-left r) h
     (r-top r) (+ v (r-width r))
     (r-right r) (+ h (r-width r))
     (r-bottom r) (+ v (r-height r))))


(defun nr-scale (r factor)
   (nr-make r
     (round (* (r-left r) factor))
     (round (* (r-top r) factor))
     (round (* (r-right r) factor))
     (round (* (r-bottom r) factor))))

(defun r-empty (r)
  (or (zerop (r-width r))
    (zerop (r-height r))))

(defun r-width (r) (abs (- (r-right r)(r-left r))))
(defun r-height (r) (abs (- (r-top r)(r-bottom r))))
(defun r-area (r) (* (r-width r)(r-height r)))

(defun nr-offset (r dh dv)
;;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; (declare (type fixnum dh dv))
  (incf (r-left r) dh)
  (incf (r-right r) dh)
  (incf (r-top r) dv)
  (incf (r-bottom r) dv)
  r)

(defun nr-outset (box dh &optional (dv dh))
;;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum dh dv))
  (decf (r-left box) dh)
  (incf (r-right box) dh)
  (decf (r-top box) dv)
  (incf (r-bottom box) dv)
  box)

(defun r-bounds (box)
  (list (r-left box)(r-top box)(r-right box)(r-bottom box)))

(defun pt-in-bounds (point bounds-left bounds-top bounds-right boundsbottom)
;;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum bounds-left bounds-top bounds-right boundsbottom))
  (and (<= bounds-left (progn (v2-h point)) bounds-right)
       (<= bounds-top (progn (v2-v point)) boundsbottom)))


(defun r-in-bounds (box bounds-left bounds-top bounds-right boundsbottom)
;;;   (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum bounds-left bounds-top bounds-right boundsbottom))
  (and (<= bounds-left (progn (r-left box)) (progn (r-right box)) bounds-right)
       (<= bounds-top (progn (r-top box)) (progn (r-bottom box)) boundsbottom)))

(defun r-unitize (object-r unit-r &aux (ww (r-width unit-r))(wh (r-height unit-r)))
  (flet ((cf (i) (coerce i 'float)))
    (mkr (cf (/ (- (r-left object-r)(r-left unit-r)) ww))
      (cf (/ (- (r-top unit-r)(r-top object-r)) wh))
      (cf (/ (- (r-right object-r)(r-left unit-r)) ww))
      (cf (/ (- (r-top unit-r)(r-bottom object-r)) wh)))))

(defun r-scale (r x y)
  (mkr (* (r-left r) x)
    (* (r-top r) y)
    (* (r-right r) x)
    (* (r-bottom r) x)))

(defun r-analog (this1 that1 this2)
  (mkr (* (r-left this2) (/ (r-left that1)(r-left this1)))
    (* (r-top this2) (/ (r-top that1)(r-top this1)))
    (* (r-right this2) (/ (r-right that1)(r-right this1)))
    (* (r-bottom this2) (/ (r-bottom that1)(r-bottom this1)))))


;;; --- Up / Down variability management ---

(eval-now!
  (export '(*up-is-positive* ups ups-more ups-most downs downs-most downs-more)))

(defparameter *up-is-positive* t
  "You should set this to NIL for most GUIs, but not OpenGl")

(defun ups (&rest values)
  (apply (if *up-is-positive* '+ '-) values))

(defun ups-more (&rest values)
  (apply (if *up-is-positive* '> '<) values))

(defun ups-most (&rest values)
  (apply (if *up-is-positive* 'max 'min) values))

(defun downs (&rest values)
  (apply (if *up-is-positive* '- '+) values))

(defun downs-most (&rest values)
  (apply (if *up-is-positive* 'min 'max) values))

(defun downs-more (&rest values)
  (apply (if *up-is-positive* '< '>) values))

