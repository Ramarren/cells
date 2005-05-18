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

(defun c-link-ex (used &aux (user (car *c-calculators*)))
  (c-assert user)
  (assert used)
  (when (or (c-optimized-away-p used)
          (not (typep used 'cell)))
    (return-from c-link-ex nil))


  ;
  ; --------- debug stuff --------------
  (c-assert user)
  (c-assert (c-model user))
  (c-assert (c-model used))
  (c-assert (not (cmdead user)) () "dead user in link-ex ~a, used being ~a" user used)
  (c-assert (not (cmdead used)) () "dead used in link-ex ~a, user being ~a" used user)

  #+dfdbg (trc user "c-link > user, used" user used)
  (c-assert (not (eq :eternal-rest (md-state (c-model user)))))
  (c-assert (not (eq :eternal-rest (md-state (c-model used)))))
  (count-it :c-link-entry)

;;;  (loop for ku in (c-usesds user)
;;;        for posn upfrom 0
;;;        wh

;;;  (loop with prior-used = 0
;;;        and found = nil
;;;        for known-used in (c-useds user)
;;;        when (eq known-used used)
;;;        do (progn
;;;             (setf found t)
;;;             (loop-finish))
;;;        finally (return (- *cd-usagect*
;;;                (- (length (cd-useds user))
;;;                  (or (position used (cd-useds user)) 0)))))
        
  (if (find used (c-useds user))
      (count-it :known-used)
    (progn
      (trc nil "c-link > new user,used " user used)
      (count-it :new-used)
      (push user (c-users used))
      (push used (cd-useds user))))

  (let ((mapn (get-mapn used (cd-useds user))
          #+not (- *cd-usagect*
                  (- (length (cd-useds user))
                    (or (position used (cd-useds user)) 0)))))
    ;; (trc user "c-link> setting usage bit" user mapn used)
    (if (minusp mapn)
        (c-break "whoa. more than ~d used by ~a? i see ~d"
          *cd-usagect* user (length (cd-useds user)))
      (cd-usage-set user mapn)))
  used)

#+TEST
(dotimes (n 3)
  (trc "mapn" n (get-mapn n '(0 1 2))))

(defun get-mapn (seek map)
  (- *cd-usagect*
    (loop with seek-pos = nil
          for m in map
          for pos upfrom 0
          counting m into m-len
          when (eql seek m)
          do (setf seek-pos pos)
          finally (return (- m-len seek-pos)))))

;--- c-unlink-unused --------------------------------

(defun c-unlink-unused (c &aux (usage (cd-usage c)))
  (loop for useds on (cd-useds c)
        for used = (car useds)
        for mapn upfrom (- *cd-usagect* (length (cd-useds c)))
        when (zerop (sbit usage mapn))
        do
        (c-assert (not (minusp mapn)))
        (c-assert (< mapn *cd-usagect*))

        (trc nil "dropping unused" used :mapn-usage mapn usage)
        (count-it :unlink-unused)
        (c-unlink-user used c)
        (rplaca useds nil))
  (setf (cd-useds c) (delete-if #'null (cd-useds c))))

(defun c-user-path-exists-p (from-used to-user)
  (count-it :user-path-exists-p)
  (or (find to-user (c-users from-used))
    (find-if (lambda (from-used-user)
               (c-user-path-exists-p from-used-user to-user))
      (c-users from-used))))

; ---------------------------------------------

(defun cd-usage-set (c mapn)
  (setf (sbit (cd-usage c) mapn) 1))

(defun cd-usage-clear-all (c)
  (bit-and (cd-usage c)
           #*0000000000000000000000000000000000000000000000000000000000000000
           t))

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
  #+dfdbg (trc user "user unlinking from used" user used)
  (setf (c-users used) (delete user (c-users used)))
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

