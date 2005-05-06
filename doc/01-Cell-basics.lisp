;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cellsS -*-
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

#|

here is a minimal primer on cells, just enough for you to
keep up with the next tutorial. that will be a substantial project
in which we develop a clos object inspector.

the inspector project will give you a feel for what it is like to 
program with cells and cello /after/ you are fluent in the
technology. the intent is not to teach you cello, rather to
motivate your learning it.

so why the primer on cells? if things like c? and cv and def-c-output 
do not mean anything to you, the hunh? factor will be overwhelming.


cells
-----
think of a clos slot as a cell in a paper spreadsheet, a financial
modeling tool popular enough to make visi-calc the first business
killer app for microcomputers.

as a child i watched my father toil at home for hours over paper 
spreadsheets with pencil and slide rule. after he changed one value, 
he had to propagate that change to other cells by first remembering 
which other ones included the changed cell in their computation. 
then he had to do the calculations for those, erase, enter...
and then repeating that process to propagate those changes in a 
cascade across the paper.

visi-calc let my father take the formula he had in mind and 
put it in (declare it to) the electronic spreadsheet. then visi-calc 
could do the tedious work: recalculating, knowing what to recalculate, 
and knowing in what order to recalculate.

cells do for programmers what electronic spreadsheets did for my father.
without cells, clos slots are like cells of a paper spreadsheet. 
a single key-down event can cause a cascade of change throughout an 
application. the programmer has to arrange for it all to happen,
all in the right order: delete any selected text, insert 
the new character, re-wrap the text, update the undo mechanism, revisit
the menu statuses ("cut" is no longer enabled), update the scroll bars,
possibly scroll the window, flag the file as unsaved...

with cells, the programmer looks at program state differently. one
asks, "how could i compute, at any point of runtime, a value for 
a given slot of an arbitrary instance, based only on other runtime state 
(other slots of other instances)." great fun, by the way, as well as
enforcing good programming practices like encapsulation.

an example will help. consider indeed the state of the "cut" menu item. 
in some applications, programmers have a dozen places in their code
where they tend to the status of the cut menu item. one might be:

(defun do-clear (edit-structure)
  (when (selected-range edit-structure)
    <set up undo>
    <toss selected text>
    <etc><etc>
    (menu-item-enable *edit-cut* nil)
    (menu-item-enable *edit-copy* nil)
    (menu-item-enable *edit-clear* nil)))

other programmers wait until the user clicks on the edit menu, 
then decide just-in-time from program state whether the cut item 
should be enabled:

(defmethod prep-for-display ((m edit-menu))
  <lotsa other stuff>
  (when (typep (focus *app*) 'text-edit-widget)
    (menu-item-enable (find :cut (items m) :key #'item-name)
      (not (null (selected-range (focus *app*)))))))

this latter programmer is ready for cells, because they
have already shifted from imperative to declarative thinking;
they have learned to write code that works based not on what 
has happened lately, but instead only on the current program 
state (however it got that way). 

the cell programmer writes:

(make-instance 'menu-item
  :name :cut
  :label "cut"
  :cmd-key +control-x+
  :actor #'do-cut
  :enabled (c? (when (typep (focus *app*) 'text-edit-widget)
                 (not (null (selected-range (focus *app*)))))))

...and now they can forget the menu item exists as they work
on the rest of the application. the menu-item enabled status
will stay current (correct) as the selected-range changes
and as the focus itself changes as the user moves from field
to field.

that covers the spirit of cells. now let's look at the syntax
and mechanics, with examples you can execute once you have 
loaded the cells package. see the read-me.txt file in the
root directory into which the cello software was unzipped.

we'll model a falling stone, where the distance fallen is half
the product of the acceleration (due to gravity) and the
square of the time falling.

|#

(in-package :cells)

(defmodel stone ()
  ((accel :cell t :initarg :accel :initform 0 :accessor accel)
   (time-elapsed :cell t :initarg :time-elapsed
     :initform (c-in 0)
     :accessor time-elapsed)
   (distance :cell t :initarg :distance :initform 0 :accessor distance))
  (:default-initargs
      :distance (c? (/ (* (accel self)
                         (expt (time-elapsed self) 2))
                      2))))

(def-c-output accel ((self stone) new old old-bound-p)
  (trc "echo accel" :new new :old old :oldp old-bound-p)) ;; TRC provides print diagnostics

(def-c-output time-elapsed ((self stone)) ;; short form (I'm lazy)
  (trc "echo time-elapsed" :new new-value :old old-value :oldp old-value-boundp))

(def-c-output distance ((self stone))
  (format t "~&echo distance fallen: ~d feet" new-value))


#|
let's look at non-standard syntax found in the forms above,
in the order in which they appear:

    (defmodel ...

defmodel is just a defclass wrapper which also sets up plumbing for cells.

   ... :cell t ...

without this option, a model instance slot cannot be powered
by a cell (and cell slot access overhead is avoided). 

with this option, one can specify what kind of cell
is to be defined: ephemeral, delta or t (normal). we'll leave 
those esoteric cell slot types for another tutorial and just 
specify t to get normal cells (the ones used 99% of the time). 

   time-elapsed ... :initform (c-in 0)...

(c-in <value>) allows the cellular slot (or "cell", for short) 
to be setf'ed. these are inputs to the dataflow,
which usually flows from c? to c? but has to start somewhere. 
since modern interactve applications are event-driven, in
real-world cello apps most cv dataflow inputs are slots closely
corresponding to some system value, such as the position slots
of a cell-powered mouse class. moving on...

a naked value such as the 32 supplied for accel cannot be changed; a 
runtime error results from any such attempt. this makes cells faster,
because some plumbing can be skipped: no dependency gets recorded between
the distance traveled and the acceleration. on the other hand, a more
elaborate model might have the acceleration varying according to the distance
between the stone and earth (in which case we get into an advance
topic for another day, namely how to handle circularity.)

next: (:default-initargs
         :distance (c? (/ (* (accel self)
                             (expt (time-elapsed self) 2))
                          2)

c? associates a rule with a cellular slot (or "cell", for short). any
read operation on another cell (directly or during a function call)
establishes a dependency of distance on that cell -- unless that cell
can never change. why would a cell not be able to change?

cell internals enforce a rule that a cell with a naked value (ie, not wrapped 
in cv or c?) cannot be changed by client code (ok, (setf slot-value) is a backdoor).
cell internals enforce this, simply to make possible the optimization
of leaving off the overhead of recording a pointless dependency.

next: (def-c-output...

here is the signature for the def-c-output macro:

   (defmacro def-c-output (slotname (&optional (self-arg 'self)
                                    (new-varg 'new-value)
                                    (oldvarg 'old-value)
                                    (oldvargboundp 'old-value-boundp))
                      &body echo-body) ....)

def-c-output defines a generic method one can specialize on any of the four
parameters. the method gets called when the slot value changes, and during 
initial processing by:

    (to-be....)

to-be brings a new model instance to life, including calling
any echos defined for cellular slots. 

why not just do this in initialize-instance? we build complex 
models in the form of a tree of many model instances, any of 
which may depend on some other model instance to calculate 
some part of its state. models find the one they are curious 
about by searching the tree.

this means we cannot just bring a model instance to life at
make-instance time; some cell rule may go looking for another
model instance. we must wait until the instance is 
embedded in the larger model tree, then we can kick off to-be.

likewise, when we yank an instance from the larger model we
will call not-to-be on it.

the good news is that unless i am doing little tutorial examples
i never think about calling to-be. trees are implemented in part
by a "kids" (short for "children") cell. the echo on that cell
calls to-be on new kids and not-to-be on kids no longer in the list.

now evaluate the following:

|#

(defparameter *s2* (make-instance 'stone
                     :accel 32 ;; (constant) feet per second per second
                     :time-elapsed (c-in 0)))

#|

...and observe:
0> echo accel :new 32 :old nil :oldp nil
0> echo time-elapsed :new 0 :old nil :oldp nil
echo distance fallen: 0 feet


getting back to the output shown above, why echo output on a new instance?

when we call to-be we want the instance to come to life. that means 
evaluating every rule so the dependencies get established, and 
propagating cell values outside the model (by calling the echo
methods) to make sure the model and outside world (if only the
system display) are consistent.

;-----------------------------------------------------------
now let's get moving:

|#

(setf (time-elapsed *s2*) 1)

#|
...and observe:
0> echo time-elapsed :new 1 :old 0 :oldp t
echo distance fallen: 16 feet

behind the scenes:
- the slot value time-elapsed got changed from 0 to 1
- the time-elapsed echo was called
- dependents on time-elapsed (here just distance) were recalculated
- go to the first step, this time for the distance slot

;-----------------------------------------------------------
to see some optimizations at work, set the cell time-elapsed to
the same value it already has:
|# 

(setf (time-elapsed *s2*) 1) 

#| observe:
nothing, since the slot-value did not in fact change.

;-----------------------------------------------------------
to test the enforcement of the cell stricture against
modifying cells holding naked values:
|#

(handler-case
    (setf (accel *s2*) 10)
  (t (error) (trc "error is" error)
    error))

#| observe:
c-setting-debug > constant  accel in stone may not be altered..init to (c-in nil)
0> error is #<simple-error @ #x210925f2>

;-----------------------------------------------------------
nor may ruled cells be modified arbitrarily:
|#

(handler-case
    (setf (distance *s2*) 42)
  (t (error) (trc "error is" error)
    error))

#| observe:
c-setting-debug > ruled  distance in stone may not be setf'ed
0> error is #<simple-error @ #x2123e392>

;-----------------------------------------------------------
aside from c?, cv, and def-c-output, another thing you will see
in cello code is how complex views are constructed using
the family class and its slot kids. every model-object has a 
parent slot, which gets used along with a family's kids slot to
form simple trees navigable up and down.

model-objects also have slots for md-name and md-value (don't
worry camelcase-haters, that is a declining feature of my code).
md-name lets the family trees we build be treated as namespaces.
md-value just turns out to be very handy for a lot of things. for
example, a check-box instance needs some place to indicate its 
boolean state. 

now let's see family in action, using code from the handbook of
silly examples. all i want to get across is that a lot happens
when one changes the kids slot. it happens automatically, and
it happens transparently, following the dataflow implicit in the
rules we write, and the side-effects we specify via echo functions.

the silly example below just shows the summer (that which sums) getting
a new md-value as the kids change, along with some echo output. in real-world 
applications, where kids represent gui elements often dependent on
each other, vastly more can transpire before a simple push into a kids
slot has run its course.

evaluate:
|#

(defmodel summer (family)
  ()
  (:default-initargs
      :kids (c-in nil) ;; or we cannot add any addend kids later
    :md-value (c? (reduce #'+ (kids self)
                   :initial-value 0
                   :key #'md-value))))

(def-c-output .md-value ((self summer))
  (trc "the sum of the values of the kids is" new-value))

(def-c-output .kids ((self summer))
  (trc "the values of the kids are" (mapcar #'md-value new-value)))

;-----------------------------------------------------------
; now just evaluate each of the following forms one by one,
; checking results after each to see what is going on
;
(defparameter *f1* (make-instance 'summer))

#|
observe:
0> the sum of the values of the kids is 0
0> the values of the kids are nil

;----------------------------------------------------------|#

(push (make-instance 'model :md-value 1) (kids *f1*))

#| observe:
0> the values of the kids are (1)
0> the sum of the values of the kids is 1

;----------------------------------------------------------|#

(push (make-instance 'model :md-value 2) (kids *f1*))

#| observe:
0> the values of the kids are (2 1)
0> the sum of the values of the kids is 3

;----------------------------------------------------------|#

(setf (kids *f1*) nil)

#| observe:
0> the values of the kids are nil
0> the sum of the values of the kids is 0

now before closing, it occurs to me you'll need a little
introduction to the semantics of ^slot-x macros generated
by the defmodel macro. here is another way to define our stone:

|#

(setq *s2* (make-instance 'stone
                    :accel 2
                    :time-elapsed (c-in 3)
                    :distance (c? (+ (^accel) (^time-elapsed)))))

#| in the olden days of cells, when they were called
semaphors, the only way to establish a dependency
was to use some form like:

   (^some-slot some-thing)

that is no longer necessary. now any dynamic access:

(1) during evaluation of a form wrapped in (c?...)
(2) to a cell, direct or inside some function
(3) using accessors named in the defmodel form (not slot-value)

...establishes a dependency. so why still have the ^slot macros?

one neat thing about the ^slot macros is that the default
argument is self, an anaphor set up by c? and its ilk, so
one can make many rules a little easier to follow by simply
coding (^slot). another is convenient specification of
synapses on dependencies, a more advanced topic we can
ignore a while.


|#
