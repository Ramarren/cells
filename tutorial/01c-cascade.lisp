#| 

Now we have automatic state management (including change propagation)
outside the Cells model as well as in. Now lets look at cascading change
by adding another level of computation, so A->B->C.

In this case: len->area->brightness
Also: len->width->area->brightness

That leads to some complications I will discuss, but no assertions here
enforce correct behavior in re those complications. Soon. :)

|#

(defpackage #:tu-depth (:use :cl :cells))
(in-package #:tu-depth)

(defmacro start-finish (key rule)
  `(progn
     (print (list :start ,key))
     (prog1
         (progn ,rule)
       (print (list :finish ,key)))))

(defmodel rectangle ()
  ((lumens :initform 1000000 :reader lumens)
   (len :initarg :len :accessor len
     :initform (c? (start-finish :len
                     (* 2 (width self)))))
   (area :initarg :area :accessor area
     :initform (c? (start-finish :area
                     (* (len self)(width self)))))
   (width :initarg :width :accessor width
     :initform (c? (start-finish :width
                     (floor (len self) 2))))
   (brightness :reader brightness
     :initform (c? (start-finish :brightness
                     (/ (^lumens) (^area)))))
   ))

#+test
(let ((r (make-instance 'rectangle :len (c-in 100))))
  (cells::ct-assert (eql 50 (width r)))
  (cells::ct-assert (eql 5000 (area r)))
  (cells::ct-assert (eql 200 (brightness r)))
  (cells::ct-assert (= 1000 (setf (len r) 1000)))
  (cells::ct-assert (eql 500000 (area r)))
  (cells::ct-assert (eql 2 (brightness r))))

#| --- discussion ----------------------------

The output in Cells is:

(:START :AREA) 
(:START :WIDTH) 
(:finish :WIDTH) 
(:finish :AREA) 
(:START :BRIGHTNESS) 
(:finish :BRIGHTNESS) 
(CELTK::ATTEMPTING (EQL 50 (WIDTH R))) 
(CELTK::ATTEMPTING (EQL 5000 (AREA R))) 
(CELTK::ATTEMPTING (EQL 200 (BRIGHTNESS R))) 
(CELTK::ATTEMPTING (= 1000 (SETF (LEN R) 1000))) 
0> c-propagate-to-users > notifying users of | [i :=[24]LEN/#<RECTANGLE>] | (AREA WIDTH)

Notice here that the LEN cell is about to tell both the width and area to recalculate,
since area depends (of course) on len and (rather artificially) width also derives
from LEN.

ie, This example has accidentally deviated into more complexity than intended. But we are 
approaching these issues anyay, so I will leave it for now. We can always break it up
later.

Let's continue:

(:START :WIDTH) 
(:finish :WIDTH) 
(:START :AREA) 
(:finish :AREA) 

Fine, now here comes the challenge. Width is also going to tell area to recalculate:

0> c-propagate-to-users > notifying users of | [? :<vld>=[24]WIDTH/#<RECTANGLE>] | (AREA)
0> c-propagate-to-users > notifying users of | [? :<vld>=[24]AREA/#<RECTANGLE>] | (BRIGHTNESS)

Correct: Area does not actually run its rule since it already did so when notified by LEN,
 but it does propagate to brightness.

(:START :BRIGHTNESS) 
(:finish :BRIGHTNESS)  
(CELTK::ATTEMPTING (EQL 500000 (AREA R))) 
(CELTK::ATTEMPTING (EQL 2 (BRIGHTNESS R))) 

|#