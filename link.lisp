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

#+(or)
(eval-when (compile load)
 (proclaim '(optimize (speed 3) (safety 0) (space 0) (debug 0))))

(defun c-link-ex (used &aux (user (car *c-calculators*)))
  (when (c-optimized-away-p used) ;; 2005-05-21 removed slow type check that used is cell
    (return-from c-link-ex nil))
  (trc nil "c-link-ex entry: used=" used :user user)
  (multiple-value-bind (used-pos useds-len)
      (loop with u-pos
          for known in (cd-useds user)
          counting known into length
          when (eq used known)
          do
            (count-it :known-used)
            (setf u-pos length)
          finally (return (values (when u-pos (- length u-pos)) length)))

    (when (null used-pos)
      (trc nil "c-link > new user,used " user used)
      (count-it :new-used)
      (setf used-pos useds-len)
      (push used (cd-useds user))
      (user-ensure used user) ;; 060604 experiment was in unlink
      )

    (handler-case
        (setf (sbit (cd-usage user) used-pos) 1)
      (type-error (error)
        (declare (ignorable error))
        (setf (cd-usage user)
          (adjust-array (cd-usage user) (+ used-pos 16) :initial-element 0))
        (setf (sbit (cd-usage user) used-pos) 1))))
  used)


;--- c-unlink-unused --------------------------------

(defun c-unlink-unused (c &aux (usage (cd-usage c)))
  (when (cd-useds c)
    (let (rev-pos)
      (labels ((nail-unused (useds)
                 (flet ((handle-used (rpos)
                          (if (zerop (sbit usage rpos))
                              (progn
                                (count-it :unlink-unused)
                                (c-unlink-user (car useds) c)
                                (rplaca useds nil))
                            (progn
                              ;; moved into c-link-ex 060604 (user-ensure (car useds) c)
                              )
                            )))
                   (if (cdr useds)
                       (progn
                         (nail-unused (cdr useds))
                         (handle-used (incf rev-pos)))
                     (handle-used (setf rev-pos 0))))))
        (nail-unused (cd-useds c))
        (setf (cd-useds c) (delete-if #'null (cd-useds c)))))))

(defun c-user-path-exists-p (from-used to-user)
  (count-it :user-path-exists-p)
  (or (find to-user (c-users from-used))
    (find-if (lambda (from-used-user)
               (c-user-path-exists-p from-used-user to-user))
      (c-users from-used))))

; ---------------------------------------------


(defun cd-usage-clear-all (c)
  (loop with a = (cd-usage c)
        for bitn below (array-dimension a 0)
        do (setf (sbit a bitn) 0)))


;--- unlink from used ----------------------
                     
(defmethod c-unlink-from-used ((user c-dependent))
  (dolist (used (cd-useds user))
    #+dfdbg (trc user "unlinking from used" user used)
    (c-unlink-user used user))
  ;; shouldn't be necessary (setf (cd-useds user) nil)
  )

(defmethod c-unlink-from-used (other)
  (declare (ignore other)))

;----------------------------------------------------------

(defun c-unlink-user (used user)
  (trc nil "user unlinking from used" user used)
  (user-drop used user)
  (c-unlink-used user used))

(defun c-unlink-used (user used)
  (setf (cd-useds user) (delete used (cd-useds user))))

;----------------- link debugging ---------------------

(defun dump-users (c &optional (depth 0))
     (format t "~&~v,4t~s" depth c)
     (dolist (user (c-users c))
          (dump-users user (+ 1 depth))))

(defun dump-useds (c &optional (depth 0))
     ;(c.trc "dump-useds> entry " c (+ 1 depth))
     (when (zerop depth)
          (format t "x~&"))
     (format t "~&|usd> ~v,8t~s" depth c)
     (when (typep c 'c-ruled)
          ;(c.trc "its ruled" c)
          (dolist (used (cd-useds c))
               (dump-useds used (+ 1 depth)))))

