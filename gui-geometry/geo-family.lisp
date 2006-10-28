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
  (export '(geo-inline-lazy)))

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
      :lr (c? (+ (^outset)
                (ecase (orientation self)
                  (:vertical (loop for k in (^kids)
                                 maximizing (l-width k)))
                  (:horizontal (bif (lk (last1 (^kids)))
                                 (pr lk) 0)))))
    :lb (c? (+ (- (^outset))
              (ecase (orientation self)
                (:vertical (loop for k in (^kids)
                                 unless (collapsed k)
                                 minimizing (pb k)))
                (:horizontal (downs (loop for k in (^kids)
                                        maximizing (l-height k)))))))
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
                                 (mk-kid-slot (px)
                                   (c? (px-maintain-pl
                                        (^prior-sib-pr self (spacing .parent)))))))))))

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


#| archive

(defmodel geo-row-fv (family-values geo-row)())
(defmodel geo-inline-fv (family-values geo-inline)())

;-------------------------- IMMatrix ------------------------------------------

(defmodel im-matrix (geo-zero-tl)
  ((columns :cell nil :initarg :columns :initform nil :accessor columns)
   (indent-hz :cell nil :initarg :indent-hz :initform 0 :accessor indent-hz)
   (spacing-hz :cell nil :initarg :spacing-hz :initform 0 :accessor spacing-hz)
   (spacing-vt :cell nil :initarg :spacing-vt :initform 0 :accessor spacing-vt))
  (:default-initargs
      :kid-slots (lambda (self)
                   (declare (ignore self))
                   (list
                    (mk-kid-slot (px)
                                 (c? (let ((parent (fm-parent self)))
                                       (+ (indent-hz parent)
                                          (if (zerop (mod (fm-pos self)
                                                          (or (columns parent)
                                                              (length (kids parent)))))
                                              0
                                            (+ (spacing-hz parent)
                                               (pr (find-prior self (kids parent)))))))))
                    (mk-kid-slot (py)
                                 (c? (let* ((parent (fm-parent self))
                                            (psib (find-prior self (kids parent))))
                                       (if (and psib (columns parent))
                                           (if (zerop (mod (fm-pos self) (columns parent)))
                                               (+ (- (abs (spacing-vt parent))) (pb psib))
                                             (pt psib))
                                         0))))))))

;--------------- IGRowFlow ----------------------------

(defmodel geo-row-flow (geo-row)
  ((spacing-hz :cell nil :initarg :spacing-hz :initform 0 :reader spacing-hz)
   (spacing-vt :cell nil :initarg :spacing-vt :initform 0 :reader spacing-vt)
   (aligned :cell nil :initarg :aligned :initform nil :reader aligned))
  (:default-initargs
   :lb  (c? (geo-kid-wrap self 'pb))
    :kid-slots (lambda (self)
                 (declare (ignore self))
                 (list
                  (mk-kid-slot (py)
                    (c? (py-maintain-pt
                         (let ((ph (^prior-sib-pr self (spacing-hz .parent) (aligned .parent))))
                           (if (> (+ ph (l-width self)) (l-width .parent))
                               (^prior-sib-pb self (spacing-vt .parent))
                             (^prior-sib-pt self))))))
                  (mk-kid-slot (px)
                    (c? (px-maintain-pl
                         (let ((ph (^prior-sib-pr self (spacing-hz .parent) (aligned .parent))))
                           (if (> (+ ph (l-width self)) (l-width .parent))
                               0
                             ph)))))))))

|#
