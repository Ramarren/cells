;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
;;;
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

;;
;; OK, nothing new here, just some old example code I found lying around. FWIW...
;;

(defmodel boiler1 ()
  ((id :cell nil :initarg :id :accessor id :initform (random 1000000))
   (status :initarg :status :accessor status :initform nil) ;; vanilla cell
   (temp :initarg :temp :accessor temp :initform nil)
   (vent :initarg :vent :accessor vent :initform nil)
   ))

(defun boiler-1 ()

  ;; resets debugging/testing specials
  (cell-reset)   

  (let ((b (make-instance 'boiler1
             :temp  (c-in 20)
             :status (c? (if (< (temp self) 100)
                              :on
                            :off))
             :vent (c? (ecase (^status) ;; expands to (status self) and also makes coding synapses convenient
                          (:on :open)
                          (:off :closed))))))

    (cv-assert (eql 20 (temp b)))
    (cv-assert (eql :on (status b)))
    (cv-assert (eql :open (vent b)))

    (setf (temp b) 100) ;; triggers the recalculation of status and then of vent

    (cv-assert (eql 100 (temp b)))
    (cv-assert (eql :off (status b)))
    (cv-assert (eql :closed (vent b)))
    ))

#+test
(boiler-1)

;
; now let's see how output functions can be used...
; and let's also demonstrate inter-object dependency by 
; separating out the thermometer
;

;;; note that thermometer is just a regular slot, it is
;;; not cellular.

(defmodel boiler2 ()
  ((status :initarg :status :accessor status :initform nil)
   (vent :initarg :vent :accessor vent :initform nil)
   (thermometer :cell nil :initarg :thermometer :accessor thermometer :initform nil)
   ))

;;; def-c-output ((slot-name) (&optional method-args) &body body

;;; the def-c-output macro defines a method with
;;; three arguments -- by default, these arguments are named
;;;   self -- bound to the instance being operated on
;;;   old-value -- bound to the previous value of the cellular slot
;;;     named slot-name, of the instance being operated on.
;;;   new-value -- bound to the new value of said cellular slot

;;; (this is why the variables self, old-value, and new-value can exist
;;; below in the body, when it appears they are not defined in any
;;; lexical scope)

;;; the body of the macro defines code which is executed
;;; when the the slot-name slot is initialized or changed.

(def-c-output status ((self boiler2))
  (trc "output> boiler status" self :oldstatus= old-value :newstatus= new-value)
  ;
  ; << in real life call boiler api here to actually turn it on or off >>
  ;
  )

(def-c-output vent ((self boiler2))
  (trc "output> boiler vent changing from" old-value :to new-value)
  ;
  ; << in real life call boiler api here to actually open or close it >>
  ;
  )


(defmodel quiet-thermometer ()
  ((temp :initarg :temp :accessor temp :initform nil)
   ))

(defmodel thermometer (quiet-thermometer)())

;;; notice instead of oldvalue and newvalue, here the
;;; old and new values are bound to parameters called oldtemp
;;; and newtemp

(def-c-output temp ((self thermometer) newtemp oldtemp)
  (trc "output> thermometer temp changing from" oldtemp :to newtemp))

;--------------------------


;;; here we introduce the to-be-primary construct, which causes
;;; immediate initialization of cellular slots.

;;; notice how the status cell of a boiler2 can depend
;;; on the temp slot of a thermometer, illustrating how
;;; dependencies can be made between the cellular slots of
;;; instances of different classes.


(defun boiler-2 ()
  (cell-reset)    
  (let ((b (make-instance 'boiler2 
                    :status (c? (eko ("boiler2 status c?")
                                     (if (< (temp (thermometer self)) 100)
                                         :on :off)))
                    :vent (c? (ecase (^status)
                                 (:on :open)
                                 (:off :closed)))
                    :thermometer (make-instance 'thermometer
                                   :temp (c-in 20)))))
                   
    (cv-assert (eql 20 (temp (thermometer b))))
    (cv-assert (eql :on (status b)))
    (cv-assert (eql :open (vent b)))
    
    (setf (temp (thermometer b)) 100)
    
    (cv-assert (eql 100 (temp (thermometer b))))
    (cv-assert (eql :off (status b)))
    (cv-assert (eql :closed (vent b)))
    ))

#+test
(boiler-2)

;;; ***********************************************
;;; ***********************************************
;;; ***********************************************

#|          intro to cells, example 3        |# 

;;; ***********************************************
;;; ***********************************************
;;; ***********************************************


;;; note:  we use boiler2 and thermometer from example 2 in example 3,
;;; along with their def-output methods defined in example 2.
;;;
;;; also: these do not use cv-assert to perform automatic testing, but
;;; they do illustrate a possible real-world application of synapses. to
;;; observe the difference made by synapses, one must look at the trace output
;
; now let's look at synapses, which mediate a dependency between two cells.
; the example here has an input argument (sensitivity-enabled) which when
; enables gives the temp cell an (fsensitivity 0.05) clause.

; the example simulates a thermometer perhaps
; malfunctioning which is sending streams of values randomly plus or minus
; two-hundredths of a degree. does not sound serious, except...
;
; if you run the example as is, when the temperature gets to our on/off threshhold
; of 100, chances are you will see the boiler toggle itself on and off several times
; before the temperature moves away from 100.
;
; building maintenance personel will report this odd behavior, probably hearing the
; vent open and shut and open again several times in quick succession.

; the problem is traced to the cell rule which reacts too slavishly to the stream
; of temperature values. a work order is cut to replace the thermometer, and to reprogram
; the controller not to be so slavish. there are lots of ways to solve this; here if
; you enable sensitivity by running example 4 you can effectively place a synapse between the
; temperature cell of the thermometer and the status cell of the boiler which
; does not even trigger the status cell unless the received value differs by the
; specified amount from the last value which was actually relayed.

; now the boiler simply cuts off as the temperature passes 100, and stays off even if
; the thermometer temperature goes to 99.98. the trace output shows that although the temperature
; of the thermometer is changing, only occasionally does the rule to decide the boiler
; status get kicked off.
;



(defun boiler-3 (&key (sensitivity-enabled t))
  (declare (ignorable sensitivity-enabled))
  (cell-reset) 
  #+soon
  (let ((b (make-instance 'boiler2 
              :status (c? (let ((temp (if sensitivity-enabled
                                          (temp (thermometer self) (f-sensitivity 0.05))
                                        (temp (thermometer self)))))
                            ;;(trc "status c? sees temp" temp)
                            (if (<  temp 100) :on :off)
                            ))
              :vent (c? (ecase (^status) (:on :open) (:off :closed)))
              :thermometer (make-instance 'quiet-thermometer :temp (c-in 20))
              )))
    ;
    ; let's simulate a thermometer which, when the temperature is actually
    ; any given value t will indicate randomly anything in the range
    ; t plus/minus 0.02. no big deal unless the actual is exactly our
    ; threshold point of 100...
    ;
    (dotimes (x 4)
      ;;(trc "top> ----------- set base to" (+ 98 x))
      (dotimes (y 10)
        (let ((newtemp (+ 98 x (random 0.04) -.02))) ;; force random variation around (+ 98 x)
          ;;(trc "top> ----------- set temp to" newtemp)
          (setf (temp (thermometer b)) newtemp))))))


(defun boiler-4 () (boiler-3 :sensitivity-enabled t))

;;
;; de-comment 'trc statements above to see what is happening
;;
#+test
(boiler-3)

#+test
(boiler-4)

(defun boiler-5 ()

  (cell-reset) 
  #+soon
  (let ((b (make-instance 'boiler2 
              :status (c-in :off)
              :vent (c? (trc "caculating vent" (^status))
                      (if (eq (^status) :on)
                          (if (> (temp (thermometer self) (f-debug 3)) 100)
                              :open :closed)
                        :whatever-off))
              :thermometer (make-instance 'quiet-thermometer
                             :temp (c-in 20)))))

    (dotimes (x 4)
      (dotimes (n 4)
        (incf (temp (thermometer b))))
      (setf (status b) (case (status b) (:on :off)(:off :on))))))

#+test

(boiler-5)

(defun f-debug (sensitivity &optional subtypename)
  (declare (ignore sensitivity subtypename))
  #+soon
  (mk-synapse (prior-fire-value)
    :fire-p (lambda (syn new-value)
              (declare (ignorable syn))
              (eko ("fire-p decides" prior-fire-value sensitivity)
                (delta-greater-or-equal
                 (delta-abs (delta-diff new-value prior-fire-value subtypename) subtypename)
                 (delta-abs sensitivity subtypename) 
                 subtypename)))
    
    :fire-value (lambda (syn new-value)
                   (declare (ignorable syn))
                   (eko ("f-sensitivity relays")
                     (setf prior-fire-value new-value)) ;; no modulation of value, but do record for next time
                   )))