(in-package :cells)

(defmd tcp ()
  (left (c-in 0))
  (top (c-in 0))
  (right (c-in 0))
  (bottom (c-in 0))
  (area (c? (trc "area running")
          (* (- (^right)(^left))
              (- (^top)(^bottom))))))

(defobserver area ()
  (TRC "new area" new-value old-value old-value-boundp :pulse *data-pulse-id*))

(defobserver bottom ()
  (TRC "new bottom" new-value old-value old-value-boundp :pulse *data-pulse-id*)
  (with-integrity (:change 'bottom-tells-left)
    (setf (^left) new-value)))

(defobserver left ()
  (TRC "new left" new-value old-value old-value-boundp :pulse *data-pulse-id*))

(defun tcprop ()
  (untrace)
  (ukt:test-prep)
  (LET ((box (make-instance 'tcp)))
    (trc "changing top to 10" *data-pulse-id*)
    (setf (top box) 10)
    (trc "not changing top" *data-pulse-id*)
    (setf (top box) 10)
    (trc "changing right to 10" *data-pulse-id*)
    (setf (right box) 10)
    (trc "not changing right" *data-pulse-id*)
    (setf (right box) 10)
    (trc "changing bottom to -1" *data-pulse-id*)
    (decf (bottom box))
    (with-one-datapulse ()
      (loop repeat 5 do
            (trc "changing bottom by -1" *data-pulse-id*)
            (decf (bottom box))))))
  




