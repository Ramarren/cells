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

     
  (unless (find used (c-useds user))
    (trc nil "c-link > new user,used " user used)
    (c-add-user used user)
    (c-add-used user used))

  (let ((mapn (- *cd-usagect*
                (- (length (cd-useds user))
                  (or (position used (cd-useds user)) 0)))))      
    ;; (trc user "c-link> setting usage bit" user mapn used)
    (if (minusp mapn)
        (c-break "whoa. more than ~d used by ~a? i see ~d"
          *cd-usagect* user (length (cd-useds user)))
      (cd-usage-set user mapn)))
  used)

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
        (c-unlink-user used c)
        (rplaca useds nil))
  (setf (cd-useds c) (delete-if #'null (cd-useds c))))

(defun c-add-user (used user)
  (count-it :c-adduser)
  (pushnew user (c-users used))
  used)

(defun c-user-path-exists-p (from-used to-user)
  (count-it :user-path-exists-p)
  (or (find to-user (c-users from-used))
    (find-if (lambda (from-used-user)
               (c-user-path-exists-p from-used-user to-user))
      (c-users from-used))))

; -----------

(defun c-add-used (user used)
  (count-it :c-used)
  #+ucount (unless (member used (cd-useds user))
             (incf *cd-useds*)
             (when (zerop (mod *cd-useds* 100))
               (trc "useds count = " *cd-useds*)))
  (pushnew used (cd-useds user))
  (trc nil "c-add-used>  user <= used" user used (length (cd-useds user)))
  (cd-useds user))

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

