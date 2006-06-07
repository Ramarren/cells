;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells -*-
;;;
;;; Copyright © 2004 by Bill Clementson
;;;
;;; Reprinted, reformatted, and modestly revised by permission.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
;;; IN THE SOFTWARE.

#|

Experimenting with Cells
----------------------------
Thursday, September 11, 2003

Kenny Tilton has been talking about his Cells implementation on comp.lang.lisp for some time 
but I've only just had a look at it over the past few evenings. It's actually pretty neat. 
Kenny describes Cells as, conceptually, analogous to a spreadsheet cell (e.g. -- something 
in which you can put a value or a formula and have it updated automatically based on changes 
in other "cell" values). Another way of saying this might be that Cells allows you to define 
classes whose slots can be dynamically (and automatically) updated and for which standard 
observers can be defined that react to changes in those slots.

Hmmm, maybe an example works best. Here's one that's a variation on one of the examples 
included in the latest distribution. I'll create a "motor" object that reacts to changes 
in the motor's operating temperature. If the temperature exceeds 100 degrees, the motor will 
need to be shut off. If it is shut off, the flow from the fuel pump will also need to be 
closed (otherwise, we get a big pool of fuel on the floor).

So, by using Cells in this example, the following will be demonstrated:

    * Create slots whose values vary based on a formula. The formula can be defined at 
      either class definition time or at object instantiation time.

    * Dynamically (and automatically) update dependent slot variables (maintaining consistency 
      between dependent class attributes).

    * Create Observers that react to changes in slot values to handle "external" 
      actions (e.g. - GUI updates, external API calls, etc.).

    * Automatically filter slot changes so that we only update dependent slots 
      when the right granularity of change occurs.

First, define the motor class (Note: defmodel is a macro that wraps a class 
definition and several method definitions):
|#

(in-package :cells)

(defmodel motor ()
  ((status :initarg :status :accessor status :initform nil)
   (fuel-pump :initarg :fuel-pump :accessor fuel-pump 
	      :initform (c? (ecase (^status) (:on :open) (:off :closed))))
   (temp :initarg :temp :accessor temp :initform (c-in 0))))

#+test
(progn 
  (cells-reset)
  (setf (status (make-instance 'motor :status :on)) 42))

#|

Note that "status" is a cell with no initial value or formula, "fuel-pump" is 
a cell that has a formula that depends on the value of "status" (the ^status notation 
is shorthand to refer to a slot in the same instance), and "temp" is initialized to zero.

Next, define observers (this is an optional step) using a Cells macro. 
These observers act on a change in a slot's value. They don't actually update 
any dependent slots (this is done automatically by Cells and the programmer 
doesn't have to explicitly call the slot updates), they just provide a mechanism 
for the programmer to handle outside dependencies. In this example, we're just 
printing a message; however, in a real program, we would be calling out to something 
like an Allen Bradley controller to turn the motor and fuel pump on/off.

|#

(defobserver status ((self motor))
  (trc "motor status changing from" old-value :to new-value))

(defobserver fuel-pump ((self motor))
  (trc "motor fuel-pump changing from" old-value :to new-value))

(defobserver temp ((self motor))
  (trc "motor temperature changing from" old-value :to new-value))

#|

Then, create an instance of the motor. Note that we programmatically assign 
a formula to the "status" slot. The formula states that when the temperature 
rises above 100 degrees, we change the status to "off". Since the temperature may 
fluctuate around 100 degrees a bit before it moves decisively one way or 
the other (and we don't want the motor to start turning off and on as we get 
minor temperature fluctuations around the 100 degree mark), we use another 
Cells feature ("Synapses" allow for pre-defined filters to be applied to a 
slot's value before it is used to update other slots) to filter the temperatures 
for small variations. Note that the formula is being assigned to the "status" 
slot at instantiation time as this gives us the ability to create different 
formulas for different types of motors without subclassing "motor".

|#

#+evaluatethis

(defparameter *motor1*
  (make-instance 'motor 
    :status (c? (if (< (f-sensitivity :tmp (0.05) (^temp)) 100)
                  :on :off))))

#|

This alone produces the following results as the Cells engine gets the motor
instance fully active, which requires getting the real-world motor
in synch with the CLOS instance:

0> motor status changing from | NIL | :TO :ON
0> motor fuel-pump changing from | NIL | :TO :OPEN
0> motor temperature changing from | NIL | :TO 0

Then we test the operation of the motor by changing the motor's 
temperature (starting at 99 degrees and increasing it by 1 degree +/- a small random variation).

|#

#+evaluatethis

(dotimes (x 2)
  (dotimes (y 10)
    (let ((newtemp (+ 99 x (random 0.07) -.02))) 
      (setf (temp *motor1*) newtemp))))

#|

This produces the following results, which will vary from run to run because of
the use of a random amount to simulate real-world variability:

0> motor temperature changing from NIL :TO 0 
0> motor temperature changing from 0 :TO 98.99401 
0> motor temperature changing from 98.99401 :TO 99.01954 
[snipped 8 intermediate readings] 
0> motor temperature changing from 99.00016 :TO 100.00181 
0> motor status changing from :ON :TO :OFF 
0> motor fuel-pump changing from :OPEN :TO :CLOSED 
0> motor temperature changing from 100.00181 :TO 100.0177 
0> motor temperature changing from 100.0177 :TO 99.98742 
0> motor temperature changing from 99.98742 :TO 99.99313 
[snipped 6 intermediate readings] 

Notice how the fsensitivity synapse prevents minor fluctuations around 100 degrees
from causing the motor to start turning itself on and off in rapid succession,
possibly causing it to flood or fail in some way.

|#