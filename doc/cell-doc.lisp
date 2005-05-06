;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
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

Deep thoughts: Where a program implements a model using interesting, long-lived state (such
as the position of other players on a virtual soccer field in a game program), some state will
be computed off of other such state. Not everything is raw input. eg, a player might
have set himself a task such as "tackle opponent" based on a higher-level computation
of what is going on in the game, and then "current task" is both computed yet long-lived.

Spread throughout the application will be code here and code there
which makes an interesting computation using other program state ("given what I can see,
which player if any has the ball") and decides 
to do something, which may be (a) to act outside the program such as cause some component 
to be redrawn (say, to manifest its new color, in this case if a debugging hack uses
the game display to show which player the algorithm has settled on) or (b) to cache the 
observation as a guide to other algorithms. My current task "tackle opponent" controls 
inter alia the player's choices on which way to turn and how fast to run in order 
to close on the opponent. 

Whenever a program receives an input, such as the mouse position or a keystroke or 
a message over a socket connection, some computations need to be repeated. In a
multi-player game an external server will be deciding the position of the ball, and
when that changes my program must rethink a lot of things which were decided based
on the old position of the ball.

Cells's job is to make sure that last bit goes smoothly, which we will define now.

Suppose the system has reached the stable, valid state reached after 
autoinitialization of the initial model population...we'll worry about initialization
 later. I like to think of a change to a variable such as the window's width as 
a /data pulse/, or /pulse/ for short. If we enumerate these pulses sequentially,
we can state the Prime Directive of Cells as:

    take a system gets from pulse n to n+1 smoothly.

To handle concurrency, we can instead stamp pulses with the time. Then we can speak
of time T and T+1, which will be time stamps such that no pulse known to the system
has a time stamp between T and T+1. (Where we have concurrency and network latency,
some regulating scheme will have to be found to make sure everyone has had a chance
to "share" before T+1 is decided, given T and a new set of pulses. Let's duck that
for now and assume a single thread in which each pulse also moves T to T+1.) Now
we can restate the Cells manifesto:

   take a system from time T to time T+1 smoothly

Your next question should be, what does "smoothly" mean? First, some formal definitions.

Let's call the slot changed by the triggering pulse X, as in "X marks the spot" where 
the system perturbation began. X might be the mouse position as fed to the application
by the operating system. 

Now let's talk of Cells being "at" some time Tn or other. Time starts at T0. The application
makes its first model instances and brings that cluster to life, sweeping the cluster
evaluating ruled cells. Eventually they have all been computed, and we are at T1. After this
everything is Tn or Tn+1.

-- When a pulse Pn+1 occurs, it takes the system from Tn to Tn+1.

Now suppose P is a change to slot X, the mouse position of some "system" instance we 
are using to model the application environment.

-- We say slot X is now "at" time Tn+1, because it trivially reflects the value of Pn+1

If another cell happens to have used X in its most recent calculation, it needs to be
recalculated. Once it is recalculated, we say it too has reached Tn+1. And if any Cell 
did not involve in its calculation X, directly or indirectly through some other cell, 
then we also think of it as being at time T+1. It is current with pulse Pn+1 because 
Pn+1 changes nothing of relevance to it.

With those definitions in mind, here are the qualities of a smooth 
transition from T to T+1:

(1) Completeness: everyone gets to Tn+1: every internal calculation affected directly or 
indirectly by X will be recalculated. 

(1a) Completeness: any and only those Cs which actually change in value getting from Cn to Cn+1
will have that change echoed.

(2) Efficiency: only those calculations will execute. There is no reason to run a rule
if nothing affecting its outcome has changed.

(2a) Efficiency: a calculation is affected by a transition of some cell to Tn+1
iff Cn+1 is different from Cn. ie, if X actually changes and some cell A which uses
it dutifully recalculates but comes up with the same result (it might involve a min or
max function), then some other cell B which uses A does not need to be recalculated.

(3) Simplicity: calculations will run only once (no re-entrance). More efficient as well.
This may seem obvious, but certain engineering attempts have resulted in reentrance.
But then one has to worry about backtracking. The idea is to make
programming easier, so we won't ask developers to worry about re-entrance. Not 
that we are encouraging side-effects in Cell rules. Anyway....

(4) Consistency: no rule when it runs will access any cell not already at T+1.

(5) Consistency II: akin to the first, no echo of n+1 will employ any data not at Tn+1.

(6) Completeness II: Tn+2 does not happen until the transition to Tn+1 satisfies
the above requirements.

If we timestamp every Cell as it moves from Cn to Cn+1, it all just works if we
move Tn to Tn+1 and follow the above requirements.

First, Tn+1 was reached by X itself receiving pulse N+1 and becoming Xn+1. 

Rule 2 requires us to determine if pulse N+1 actually change X. In the case of
a window being resized only vertically, the reshape event will include a "new"
value for width which is the same as the old.

If X turns out not to have changed, we do not move time to Tn+1. Efficiencies 2 and 2a.

But if X has changed, we now have Tn+1 and X reaches Xn+1 trivially.

Now rule 1 requires us to recalculate all of X's users, and if one of 
those changes, likewise notify their users. Eventually everyone gets notified, so
we look good on Rule 1. 

But now we have a problem. What if A and B are users of X, but A also uses C which uses B?
A's rule, when it runs, needs to see Cn+1 to satisfy rule 4. We cannot just run the rule
for C because we do not know until B gets calculated whether /it/ will change. We know
X has changed, but maybe B will come up with the same answer as before. In which case,
by the definitions above, C is already Cn+1 and recalculating it would be a waste.

The solution is a little tricky: descend the "used" links from C looking for X. When
we come to a terminus (a c-variable which is not X), we flag that as being at n+1 and
return nil. If at any ruled node all useds return nil, we flag the ruled cell as 
being at n+1 and return nil. 

But where we get to X, we return T. Where a ruled node gets T back from any used Cell
it kicks off its own calculation, returning T iff it changes. But before returning it
echos. Should that echo involve some user-level read of some cell which is at Cn,
accessor processing will include these safeguards which check to see if any used value
is at Tn+1 and recalculate "just in time". This means we need a special variable which 
indicates when data pulse propagation is underway:

     (let ((*propagating* (setf *time* (get-internal-real-time))))....

That way if *propagating* is false there is no need to do anything but return valid
values.

Anyway, it looks as if echo requirements can be satisfied, and that completes the
picture. But we have a problem. If some cell H (for high up in the dependency graph)
uses both A and C, it is possible for X to tell A to recalculate, which will lead
to A asking C to recalculate, which will do so and tell H to recalculate, which will
ask A for its current value. Deadlock, and again this cannot be detected via lookahead
because H's rule may not branch to A until just this pulse.

The trick is that all we need from C when it gets accessed is its value. yes, we can tell
now that H must be recalculated at some point, but A has not gone after H and will not
so recalculating H can wait. If A /does/ go after H the above framework will see to 
it that H gets recalculated. But in this case H can wait (but not be forgotten).

So we simply add H to a fifo queue of deferred dependencies to be revisited before
Tn+1 can be considered attained.



|#

