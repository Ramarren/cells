;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cells)

#|

[A minimal primer on cells, last tested on march 13, 2006 against cells3]

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

(defobserver accel ((self stone) new old old-bound-p)
  (trc "observer sees accel" :new new :old old :oldp old-bound-p)) ;; TRC provides print diagnostics

(defobserver time-elapsed ((self stone)) ;; short form (I'm lazy)
  (trc "observer sees time-elapsed" :new new-value :old old-value :oldp old-value-boundp))

(defobserver distance ((self stone))
  (format t "~&observer sees distance fallen: ~d feet" new-value))


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

next: (defobserver...

here is the signature for the defobserver macro:

   (defmacro defobserver (slotname (&optional (self-arg 'self)
                                    (new-varg 'new-value)
                                    (oldvarg 'old-value)
                                    (oldvargboundp 'old-value-boundp))
                      &body observer-body) ....)

defobserver defines a generic method with method-combination progn,
which one can specialize on any of the four
parameters. the method gets called when the slot value changes, and during 
initial processing by shared-initialize (part of make-instance).

shared-initialize brings a new model instance to life, including calling
any observers defined for cellular slots. 

now evaluate the following:

|#

#+evaluatethis

(progn
  (cells-reset)
  (defparameter *s2* (make-instance 'stone
                       :accel 32 ;; (constant) feet per second per second
                       :time-elapsed (c-in 0))))

#|

...and observe:
0> observer sees accel :new 32 :old nil :oldp nil
0> observer sees time-elapsed :new 0 :old nil :oldp nil
observer sees distance fallen: 0 feet


getting back to the output shown above, why observer output on a new instance? we want 
any new instance to come fully to life. that means 
evaluating every rule so the dependencies get established, and 
propagating cell values outside the model (by calling the observer
methods) to make sure the model and outside world (if only the
system display) are consistent.

;-----------------------------------------------------------
now let's get moving:

|#

#+evaluatethis

(setf (time-elapsed *s2*) 1)

#|
...and observe:
0> observer sees time-elapsed :new 1 :old 0 :oldp t
observer sees distance fallen: 16 feet

behind the scenes:
- the slot value time-elapsed got changed from 0 to 1
- the time-elapsed observer was called
- dependents on time-elapsed (here just distance) were recalculated
- go to the first step, this time for the distance slot

;-----------------------------------------------------------
to see some optimizations at work, set the cell time-elapsed to
the same value it already has:
|# 

#+evaluatethis

(setf (time-elapsed *s2*) 1)

#| observe:
nothing, since the slot-value did not in fact change.

;-----------------------------------------------------------
to test the enforcement of the cell stricture against
modifying cells holding naked values:
|#

#+evaluatethis

(let ((*c-debug* t))
  (handler-case
      (setf (accel *s2*) 10)
    (t (error)
      (cells-reset) ;; clear a *stop* flag used to bring down a runaway  model :)
      (trc "error is" error)
      error)))

#| observe:
c-setting-debug > constant  accel in stone may not be altered..init to (c-in nil)
0> error is #<simple-error @ #x210925f2>

Without turning on *c-debug* one just gets the runtime error, not the explanation to standard output.

;-----------------------------------------------------------
nor may ruled cells be modified arbitrarily:
|#

#+evaluatethis

(let ((*c-debug* t))
  (handler-case
    (setf (distance *s2*) 42)
  (t (error)
    (cells-reset)
    (trc "error is" error)
    error)))

#| observe:
c-setting-debug > ruled  distance in stone may not be setf'ed
0> error is #<simple-error @ #x2123e392>

;-----------------------------------------------------------
aside from c?, cv, and defobserver, another thing you will see
in cello code is how complex views are constructed using
the family class and its slot kids. every model-object has a 
parent slot, which gets used along with a family's kids slot to
form simple trees navigable up and down.

model-objects also have slots for md-name and value (don't
worry camelcase-haters, that is a declining feature of my code).
md-name lets the family trees we build be treated as namespaces.
value just turns out to be very handy for a lot of things. for
example, a check-box instance needs some place to indicate its 
boolean state. 

now let's see family in action, using code from the handbook of
silly examples. all i want to get across is that a lot happens
when one changes the kids slot. it happens automatically, and
it happens transparently, following the dataflow implicit in the
rules we write, and the side-effects we specify via observer functions.

the silly example below just shows the summer (that which sums) getting
a new value as the kids change, along with some observer output. in real-world 
applications, where kids represent gui elements often dependent on
each other, vastly more can transpire before a simple push into a kids
slot has run its course.

evaluate:
|#

(defmodel summer (family)
  ()
  (:default-initargs
      :kids (c-in nil) ;; or we cannot add any addend kids later
    :value (c? (trc "val rule runs")
             (reduce #'+ (kids self)
                   :initial-value 0
                   :key #'value))))

(defobserver .value ((self summer))
  (trc "the sum of the values of the kids is" new-value))

(defobserver .kids ((self summer))
  (trc "the values of the kids are" (mapcar #'value new-value)))

;-----------------------------------------------------------
; now just evaluate each of the following forms one by one,
; checking results after each to see what is going on
;
#+evaluatethis

(defparameter *f1* (make-instance 'summer))

#|
observe:
0> the sum of the values of the kids is 0
0> the values of the kids are nil

;----------------------------------------------------------|#

#+evaluatethis

(push (make-instance 'model
        :fm-parent *f1*
        :value 1) (kids *f1*))

#| observe:
0> the values of the kids are (1)
0> the sum of the values of the kids is 1

;----------------------------------------------------------|#

#+evaluatethis

(push (make-instance 'model
        :fm-parent *f1*
        :value 2) (kids *f1*))

#| observe:
0> the values of the kids are (2 1)
0> the sum of the values of the kids is 3

;----------------------------------------------------------|#

#+evaluatethis

(setf (kids *f1*) nil)

#| observe:
0> the values of the kids are nil
0> the sum of the values of the kids is 0

now before closing, it occurs to me you'll need a little
introduction to the semantics of ^slot-x macros generated
by the defmodel macro. here is another way to define our stone:

|#

#+evaluatethis

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
