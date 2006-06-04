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

(defconstant *reference-dpi* 1440)

(let (
      (logical-dpi 96) ;;1440) 
      ; This is cello's internal dots per inch.  This value is germane only if size references are unqualified by a function call.
      ; Size references should always be qualified, as in (:pts 6), except when specifying pen widths.
      ; (Pen widths may pose a special case -- we may need to match screen pens to print pens.)
      
      (scan-resolution 300)                  
      ; This is the desired scan resolution, and the assumed resolution of all scans.
      ; Hypothetically, a scanner not capable of scanning at 300 dpi could make a big hash of this scheme.
      ; Rather than even pretend to support multiple resolutions within a study, for now we'll enforce 300 across the board.
      ; Dependencies on this spec can be identified by searching on scan-resolution.
      
      (logical-screen-resolution 96)         
      ; This is the internal logical screen resolution, which does _not_ have to equal the current LOGPIXELSX (LOGPIXELSY) value
      ; reported by GetDeviceCaps.  The original thought was that we could use this to rescale _all_ drawing on the fly.  Now that
      ; idea is being superseded by targetRes, but this functions (1) as a tacit targetRes for the outer window and (2) as a magic
      ; number to complicate debugging [we need to root out a few references in .bmp drawing, I think].
      
      ;;(printer-resolution 600)      ; /// improve #'cs-printer-resolution to bypass this.
      
      ;;(emf-resolution 600)
      
      )
  
  (declare (ignorable logical-dpi scan-resolution logical-screen-resolution printer-resolution))
  
  ; Notice the somewhat nonstandard naming convention:
  ; #'uInches takes logical inches and returns logical units (DPI)
  ; so, for instance, if logical-dpi = 1440, then (uInches 0.5) = 720.
  (defun u-round (number &optional (divisor 1))
    (multiple-value-bind (quotient remainder)
        (round number divisor)
      (declare (ignorable remainder))
      ;(assert (zerop remainder))
      ;(assert (zerop (mod quotient 15))) ;96ths
      quotient))
  

  (defun udots (dots dpi)
    (u-round (* dots logical-dpi) dpi))   ;only the first value will be used.
  
  (defun uinches (logical-inches)
    (u-round (* logical-inches logical-dpi)))   ;only the first value will be used.
  
  (defun uin (logical-inches)
    (uinches logical-inches))
  
  (defun upoints (logical-points)
    (udots logical-points  72))
  
  (defun upts (logical-points)
    (upoints logical-points))
  
  (defun u96ths (logical-96ths)
    (udots logical-96ths 96))
  
  (defun u8ths (logical-8ths)
    (udots logical-8ths 8))
  
  (defun u16ths (logical-16ths)
    (udots logical-16ths 16))
  
  (defun u32nds (logical-32nds)
    (udots logical-32nds 32))
  
  (defun u120ths (logical-120ths)
    (udots logical-120ths 120))
  
  (defun cs-logical-dpi ()
    logical-dpi)
  
  (defsetf cs-logical-dpi cs-logical-dpi-setf)
  
  (defun cs-logical-dpi-setf (new-value)
    (setf logical-dpi new-value))
  
  (defun cs-scan-resolution ()
    scan-resolution)
  
  (defun cs-logical-screen-resolution ()
    logical-screen-resolution)
  
  )




(defmethod u-cvt ((nn number) (units (eql :96ths)) )
  (u96ths nn))

(defmethod u-cvt ((nn number) (units (eql :8ths)) )
  (u8ths nn))

(defmethod u-cvt ((nn number) (units (eql :16ths)) )
  (u16ths nn))

(defmethod u-cvt ((nn number) (units (eql :32nds)) )
  (u32nds nn))

(defmethod u-cvt ((nn number) (units (eql :inches)) )
  (uinches nn))

(defmethod u-cvt ((nn number) (units (eql :points)) )
  (upoints nn))

(defmethod u-cvt (other units)
  (declare (ignore units))
  other)

(defmethod u-cvt ((nns cons) units)
  (cons (u-cvt (car nns) units)
        (u-cvt (cdr nns) units)))

(defmacro u-cvt! (nn units)
  `(u-cvt ,nn ,units))

(defun uv2 (x y u-key) (apply #'mkv2 (u-cvt (list x y) u-key)))

;-----------------

(defun os-logical-screen-dpi ()
  (break "need (win:GetDeviceCaps (device-context (screen *cg-system*)) win:LOGPIXELSX))"))
   
#+no(defun browser-target-resolution ()
  (target-resolution (find-window :clinisys)))

; set to 96 because the code is trying to do rect-frames for the header before the window is init'ed.

(let ((current-target-resolution 96))  ;initialize when main window is created  
   
   (defun set-current-target-resolution (resolution)
     #+shh(trc "setting current-target-resolution to" resolution)
     (setf current-target-resolution resolution))
   
   (defun cs-current-target-resolution ()
     current-target-resolution)
   
   (defun cs-target-res ()
     current-target-resolution)
   
   (defmacro with-target-resolution ((new-resolution) &rest body)
     (let ((old-resolution (gensym))
           )
        `(let ((,old-resolution (cs-current-target-resolution))
               )
            (prog2
              (set-current-target-resolution ,new-resolution)
                (progn ,@body)
              (set-current-target-resolution ,old-resolution)
            ))))
   )


;converts screen pixels to logical pixels given the current target resolution OR OPTIONAL OTHER RES
(defun scr2log (dots &optional (target-res (cs-target-res)))
  (round (* dots (cs-logical-dpi))
         target-res))

(defun log2scr (logv &optional (target-res (cs-target-res)))
  (floor-round (* logv target-res )
         (cs-logical-dpi)))

(defun cs-archos-dpi ()
  (cs-logical-dpi))

(defun floor-round (x &optional (divisor 1))
  (ceiling (- (/ x divisor) 1/2)))

;converts logical pixels to screen pixels given the current target resolution OR OPTIONAL OTHER RES
(defun logical-to-screen-vector (dots &optional target-res)
  (let ((convert-res (or target-res (cs-target-res))))  
    (floor-round (* dots convert-res) (cs-logical-dpi))))

(defun logical-to-screen-point (point &optional target-res)
  (mkv2
   (log2scr (v2-h point) target-res)
   (log2scr (v2-v point) target-res)))

(defun screen-to-logical-v2 (point &optional target-res)
  (mkv2
   (scr2log (v2-h point) target-res)
   (scr2log (v2-v point) target-res)))

(defun nr-screen-to-logical (logical-rect screen-rect &optional target-res)
  (nr-make logical-rect
   (scr2log (r-left screen-rect) target-res)
   (scr2log (r-top screen-rect) target-res)
   (scr2log (r-right screen-rect) target-res)
    (scr2log (r-bottom screen-rect) target-res)))

; logical-to-target is a more sensible name throughout

(defun logical-to-target-vector (dots &optional target-res)
  (log2scr dots target-res))
;--------------------------------------------------------------------------------------------

(defun r-logical-to-screen (logical-rect &optional target-res)
  (count-it :r-logical-to-screen)
  (nr-logical-to-screen (mkr 0 0 0 0) logical-rect target-res))

(defun nr-logical-to-screen (screen-rect logical-rect &optional target-res)
  (nr-make screen-rect
   (log2scr (r-left logical-rect) target-res)
   (log2scr (r-top logical-rect) target-res)
   (log2scr (r-right logical-rect) target-res)
    (log2scr (r-bottom logical-rect) target-res)))

;------------------------------------------------------------------------------------------------

;;;(defun set-scaling (window)
;;;  #+shh(trc "targetResolution" (targetRes window))
;;; 
;;;  (set-current-target-resolution (cs-logical-screen-resolution))          ;here and below, we'll probably make scalable
;;;  ;(set-current-target-resolution (cs-logical-dpi))
;;;  (let ((dc (device-context window))
;;;        (display-dpi (cs-logical-screen-resolution))                       ;... and use (targetRes window)
;;;        (logical-dpi (cs-logical-dpi)))
;;;     (os-SetMapMode dc win:MM_ISOTROPIC)
;;;     (os-SetWindowExtEx dc logical-dpi logical-dpi ct:hnull)                  
;;;     (os-SetViewportExtEx dc display-dpi display-dpi ct:hnull)))


(defun move-v2-x-y (v2 x y)
  (incf (v2-h v2) x)
  (incf (v2-v v2) y)
  v2)

(defmethod ncanvas-to-screen-point (self point)
  (ncanvas-to-screen-point (fm-parent self)
                          (move-v2-x-y point (px self) (py self))))

(defmethod res-to-res ((amount number) from-res to-res)
  (if to-res
      (round (* amount from-res) to-res)
    from-res))

(defmethod res-to-res ((point v2) from-res to-res)
  (nres-to-res (copy-v2 point) from-res to-res))

#+no-2e-h
(defmethod nres-to-res ((point v2) from-res to-res)
  (setf (v2-h point) (res-to-res (v2-h point) from-res to-res))
  (setf (v2-v point) (res-to-res (v2-v point) from-res to-res))
  point)

(defmethod res-to-res ((box rect) from-res to-res)
  (count-it :res-to-res)
  (nres-to-res (nr-copy (mkr 0 0 0 0) box) from-res to-res))

(defmethod nres-to-res :around (geo-thing from-res (to-res null))
  (declare (ignore from-res))
  geo-thing)

(defmethod nres-to-res ((box rect) from-res to-res)
  (setf (r-left box) (res-to-res (r-left box) from-res to-res))
  (setf (r-top box) (res-to-res (r-top box) from-res to-res))
  (setf (r-right box) (res-to-res (r-right box) from-res to-res))
  (setf (r-bottom box) (res-to-res (r-bottom box) from-res to-res))
  box)

(defun canvas-to-screen-box (self box)
  (count-it :canvas-to-screen-box)
  (nr-make-from-corners 
   (mkr 0 0 0 0)
   (ncanvas-to-screen-point self (r-top-left box))
   (ncanvas-to-screen-point self (r-bottom-right box))))

