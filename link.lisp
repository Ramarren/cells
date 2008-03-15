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

(defun record-caller (used)
  (when (c-optimized-away-p used) ;; 2005-05-21 removed slow type check that used is cell
    (trc nil "depender not being recorded because used optimized away" *depender* (c-value used) :used used)
    (return-from record-caller nil))
  (trc nil "record-caller entry: used=" used :caller *depender*)
  #+cool (when (and (eq :ccheck (md-name (c-model *depender*)))
          (eq :cview (md-name (c-model used))))
    (break "bingo"))
  (multiple-value-bind (used-pos useds-len)
      (loop with u-pos
          for known in (cd-useds *depender*)
          counting known into length
          when (eq used known)
          do
            (count-it :known-used)
            (setf u-pos length)
          finally (return (values (when u-pos (- length u-pos)) length)))

    (when (null used-pos)
      (trc nil "c-link > new caller,used " *depender* used)
      (count-it :new-used)
      (setf used-pos useds-len)
      (push used (cd-useds *depender*))
      (caller-ensure used *depender*) ;; 060604 experiment was in unlink
      )

    (handler-case
        (setf (sbit (cd-usage *depender*) used-pos) 1)
      (type-error (error)
        (declare (ignorable error))
        (setf (cd-usage *depender*)
          (adjust-array (cd-usage *depender*) (+ used-pos 16) :initial-element 0))
        (setf (sbit (cd-usage *depender*) used-pos) 1))))
  used)


;--- unlink unused --------------------------------

(defun c-unlink-unused (c &aux (usage (cd-usage c))
                         (usage-size (array-dimension (cd-usage c) 0))
                         (dbg nil)) ;; #+not (and (typep (c-model c) 'mathx::mx-solver-stack)
                                ;;(eq (c-slot-name c) '.kids))))
  (declare (ignorable dbg usage-size))
  (when (cd-useds c)
    (let (rev-pos)
      (labels ((nail-unused (useds)
                 (flet ((handle-used (rpos)
                          (if (or (>= rpos usage-size)
                                (zerop (sbit usage rpos)))
                              (progn
                                (count-it :unlink-unused)
                                #+save (when (eq 'mathx::progress (c-slot-name c))
                                  (trc "c-unlink-unused" c :dropping-used (car useds)) )
                                (c-unlink-caller (car useds) c)
                                (rplaca useds nil))
                            (progn
                              ;; moved into record-caller 060604 (caller-ensure (car useds) c)
                              )
                            )))
                   (if (cdr useds)
                       (progn
                         (nail-unused (cdr useds))
                         (handle-used (incf rev-pos)))
                     (handle-used (setf rev-pos 0))))))
        (trc nil "cd-useds length" (length (cd-useds c)) c)
        (nail-unused (cd-useds c))
        (setf (cd-useds c) (delete nil (cd-useds c)))))))

(defun c-caller-path-exists-p (from-used to-caller)
  (count-it :caller-path-exists-p)
  (or (find to-caller (c-callers from-used))
    (find-if (lambda (from-used-caller)
               (c-caller-path-exists-p from-used-caller to-caller))
      (c-callers from-used))))

; ---------------------------------------------

(defun cd-usage-clear-all (c)
  (setf (cd-usage c) (blank-usage-mask)))


;--- unlink from used ----------------------
                     
(defmethod c-unlink-from-used ((caller c-dependent))
  (dolist (used (cd-useds caller))
    (trc nil "unlinking from used" caller used)
    (c-unlink-caller used caller))
  ;; shouldn't be necessary (setf (cd-useds caller) nil)
  )

(defmethod c-unlink-from-used (other)
  (declare (ignore other)))

;----------------------------------------------------------

(defun c-unlink-caller (used caller)
  (trc nil "(1) caller unlinking from (2) used" caller used)
  (caller-drop used caller)
  (c-unlink-used caller used))

(defun c-unlink-used (caller used)
  (setf (cd-useds caller) (delete used (cd-useds caller))))

;----------------- link debugging ---------------------

(defun dump-callers (c &optional (depth 0))
     (format t "~&~v,4t~s" depth c)
     (dolist (caller (c-callers c))
          (dump-callers caller (+ 1 depth))))

(defun dump-useds (c &optional (depth 0))
     ;(c.trc "dump-useds> entry " c (+ 1 depth))
     (when (zerop depth)
          (format t "x~&"))
     (format t "~&|usd> ~v,8t~s" depth c)
     (when (typep c 'c-ruled)
          ;(c.trc "its ruled" c)
          (dolist (used (cd-useds c))
               (dump-useds used (+ 1 depth)))))
