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

#+(or)
(eval-when (compile load)
 (proclaim '(optimize (speed 3) (safety 0) (space 0) (debug 0))))


(defun c-link-ex (used &aux (user (car *c-calculators*)))
  (c-assert user)
  (c-assert used)
  (when (c-optimized-away-p used) ;; 2005-05-21 removed slow type check that used is cell
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

  (multiple-value-bind (used-pos useds-len)
      (loop with u-pos
          for known in (cd-useds user)
          counting known into length
            ;; do (print (list :data known length))
          when (eq used known)
          do
            (count-it :known-used)
            (setf u-pos length)
          finally (return (values (when u-pos (- length u-pos)) length)))

    (when (null used-pos)
      (trc nil "c-link > new user,used " user used)
      (count-it :new-used)
      (setf used-pos useds-len)
      ;; 050525kt - wait till eval completes (push user (c-users used))
      (push used (cd-useds user)))

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
                            (pushnew c (c-users (car useds))))))
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

