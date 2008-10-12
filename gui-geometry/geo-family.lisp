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

(export! geo-inline-lazy ^px-self-centered justify py-maintain-pt
  ^prior-sib-pb spacing lr-maintain-pr orientation)

;--------------- geo-inline -----------------------------
;
(defmodel geo-inline (geo-zero-tl)
  ((orientation :initarg :orientation :initform nil :accessor orientation
     :documentation ":vertical (for a column) or :horizontal (row)")
   (justify :initarg :justify :accessor justify
     :initform (c? (ecase (orientation self)
                     (:vertical :left)
                     (:horizontal :top))))
   (spacing :initarg :spacing :initform 0 :accessor spacing))
  (:default-initargs
      :lr (c? (if (^collapsed)
                  (^lr-width 0)
                (+ (^outset)
                  (ecase (orientation self)
                    (:vertical (loop for k in (^kids)
                                   maximizing (l-width k)))
                    (:horizontal (bif (lk (last1 (^kids)))
                                   (pr lk) 0))))))
    :lb (c? (if (^collapsed)
                  (^lb-height 0)
              (+ (- (^outset))
                (ecase (orientation self)
                  (:vertical (loop for k in (^kids)
                                 unless (collapsed k)
                                 minimizing (pb k)))
                  (:horizontal (downs (loop for k in (^kids)
                                          maximizing (l-height k))))))))
    :kid-slots (lambda (self)
                 (ecase (orientation .parent)
                   (:vertical (list
                               (mk-kid-slot (px :if-missing t)
                                 (c? (^px-self-centered (justify .parent))))
                               (mk-kid-slot (py)
                                 (c? (py-maintain-pt
                                      (^prior-sib-pb self (spacing .parent)))))))
                   (:horizontal (list
                                 (mk-kid-slot (py :if-missing t)
                                   (c? (py-self-centered self (justify .parent))))
                                 (mk-kid-slot (px :if-missing t)
                                   (c? (px-maintain-pl
                                        (^prior-sib-pr self (spacing .parent)))))))))
    ))

(defmodel geo-inline-lazy (geo-zero-tl)
  ((orientation :initarg :orientation :initform nil :accessor orientation
     :documentation ":vertical (for a column) or :horizontal (row)")
   (justify :initarg :justify :accessor justify
     :initform (c_? (ecase (orientation self)
                     (:vertical :left)
                     (:horizontal :top))))
   (spacing :initarg :spacing :initform 0 :accessor spacing))
  (:default-initargs
      :lr (c_? (+ (^outset)
                (ecase (orientation self)
                  (:vertical (loop for k in (^kids)
                                 maximizing (l-width k)))
                  (:horizontal (bif (lk (last1 (^kids)))
                                 (pr lk) 0)))))
    :lb (c_? (+ (- (^outset))
              (ecase (orientation self)
                (:vertical (bif (lk (last1 (^kids)))
                             (pb lk) 0))
                (:horizontal (downs (loop for k in (^kids)
                                        maximizing (l-height k)))))))
    :kid-slots (lambda (self)
                 (ecase (orientation .parent)
                   (:vertical (list
                               (mk-kid-slot (px :if-missing t)
                                 (c_? (^px-self-centered (justify .parent))))
                               (mk-kid-slot (py)
                                 (c_? (eko (nil "py" self (^lt) (l-height self)(psib))
                                        (py-maintain-pt
                                         (eko (nil "psib-pb")
                                           (^prior-sib-pb self (spacing .parent)))))))))
                   (:horizontal (list
                                 (mk-kid-slot (py :if-missing t)
                                   (c_? (py-self-centered self (justify .parent))))
                                 (mk-kid-slot (px)
                                   (c_? (px-maintain-pl
                                        (^prior-sib-pr self (spacing .parent)))))))))))



(defun ^prior-sib-pb (self &optional (spacing 0)) ;; just keeping with -pt variant till both converted to defun
  (bif (psib (find-prior self (kids .parent)
               :test (lambda (sib)
                       (not (collapsed sib)))))
    (eko (nil "^prior-sib-pb spc pb-psib -lt" (- (abs spacing)) (pb psib) (- (^lt)))
      (+ (- (abs spacing)) ;; force spacing to minus(= down for OpenGL)
        (pb psib)))   
      0))

(defun centered-h? ()
  (c? (px-maintain-pl (round (- (inset-width .parent) (l-width self)) 2))))

(defun centered-v? ()
  (c? (py-maintain-pt (round (- (l-height .parent) (l-height self)) -2))))

;--------------- geo.row.flow ----------------------------
(export! geo-row-flow fixed-col-width ^fixed-col-width ^spacing-hz spacing-hz
  max-per-row ^max-per-row)

(defmd geo-row-flow (geo-inline)
  (spacing-hz  0)
  (spacing-vt  0)
  (aligned :cell nil)
  fixed-col-width
  max-per-row
  (row-flow-layout
   (c? (loop with max-pb = 0 and pl = 0 and pt = 0
           for k in (^kids)
           for kn upfrom 0
           for kw = (or (^fixed-col-width) (l-width k))
           for kpr = (+ pl kw)
           when (unless (= pl 0)
                  (if (^max-per-row)
                      (zerop (mod kn (^max-per-row)))
                    (> kpr (- (l-width self) (outset self)))))
           do
             (when (> kpr (- (l-width self) (outset self)))
               (trc nil "LR overflow break" kpr :gt (- (l-width self) (outset self))))
             (when (zerop (mod kn (^max-per-row)))
               (trc nil "max/row break" kn (^max-per-row) (mod kn (^max-per-row))))
             (setf pl 0
               pt (+ max-pb (downs (^spacing-vt))))
             
           collect (cons (+ pl (case (justify self)
                                 (:center (/ (- kw (l-width k)) 2))
                                 (:right (- kw (l-width k)))
                                 (otherwise 0))) pt) into pxys
           do (incf pl (+ kw (^spacing-hz)))
             (setf max-pb (min max-pb (+ pt (downs (l-height k)))))
           finally (return (cons max-pb pxys)))))
  :lb  (c? (+ (bif (xys (^row-flow-layout))
                (car xys) 0)
             (downs (outset self))))
  :kid-slots (lambda (self)
               (declare (ignore self))
               (list
                (mk-kid-slot (px)
                  (c? (px-maintain-pl (car (nth (kid-no self) (cdr (row-flow-layout .parent)))))))
                (mk-kid-slot (py)
                  (c? (py-maintain-pt (cdr (nth (kid-no self) (cdr (row-flow-layout .parent))))))))))






