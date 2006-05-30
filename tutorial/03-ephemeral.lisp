

(defpackage #:tu-ephemeral (:use :cl :utils-kt :cells :tu-cells))
(in-package #:tu-ephemeral)


#|

Events present a problem for spreadsheet models. Suppose we have a clicked rule for a button
which says:

     :clicked (c? (point-in-rect
                   (screen-location (mouse-event *window*))
                   (bounding-box self)))

Now suppose we get a mouse-event outside the bounding box of widget X, and then in the
next application event something happens that makes the bounding box grow such that it
includes the location of the old mouse event. We need the mouse-event not to be there any more, 
because, well, events are events. It is relevant only in the moment of its creation and propagation.

Note, btw, that this must happen not as bang-bang:

   (setf (mouse-event *window*) (get-next-event) 
   (setf (mouse-event *window*) nil)

...because observers can kick off state change, and anyway SETF has interesting Cell semantics,
including observers firing. So setf-nil is a kludge, better that the Cells engine acknowledge that
events are different and accomodate them by silently reverting an event to nil as soon as it finishes
propagating.

Finally, so far this has worked out well as a slot attribute as defined at the class level, not 
instance by instance, by specifying :cell :ephemeral

|#

(defmodel rectangle ()
  ((click :cell :ephemeral :initform (c-in nil) :accessor click)
   (bbox :initarg :bbox :initform (c-in nil) :accessor bbox)
   (clicked :cell :ephemeral :accessor clicked
     :initform (c? (point-in-rect (^click)(^bbox))))))

(defun point-in-rect (p r)
  (when (and p r)
    (destructuring-bind (x y) p
        (destructuring-bind (l top r b) r
          (and (<= l x r)
            (<= b y top))))))

(defobserver click ((self rectangle) new-value old-value old-value-bound-p)
  (when new-value
    (with-integrity (:change)
      (TRC "setting bbox!!!")
      (setf (bbox self) (list -1000 1000 1000 -1000)))))

(defobserver clicked ((self rectangle) new-value old-value old-value-bound-p)
  (when new-value
    (TRC "clicked!!!!" self new-value)
    (compute-log :clicked)))

#+test
(progn
  (cells-reset)
  (let* ((starting-bbox (list 10 10 20 20))
         (r (make-instance 'rectangle 
              :bbox (c-in (list 10 10 20 20)))))
    (clear-computed)
    (setf (click r) (list 0 0))
    (assert (and (not (point-in-rect (list 0 0) starting-bbox))
              (point-in-rect (list 0 0)(bbox r))
              (verify-not-computed :clicked)))))

#|
The assertion demonstrates... well, it is complicated. Point 0-0 is
in the current bbox, but the system correctly determines that it
was not clicked. The click event at 0-0 happened when the bbox
was elsewhwer. When the bbox moved, the Cells engine had already cleared
the "ephemeral" click.

Note that now we have less transparency: if one wants to perturb the data model
from with an observer of some ongoing perturbation, one needs to arrange for
that nested perturbation to wait until the ongoing one completes. That
explains the "with-integrity" macro.

|#
    