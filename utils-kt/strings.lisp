;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils-kt; -*-
#|

    Utils-kt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#


(in-package :utils-kt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(case$ strloc$  make$  space$  char$  conc-list$  conc$
             left$  mid$  seg$  right$  insert$  remove$
             trim$  trunc$  abbrev$  empty$ find$  num$
             normalize$  down$  lower$  up$  upper$  equal$
              min$  numeric$  alpha$  assoc$  member$  starts$
             +return$+ +lf$+ case-string-equal)))

(defmacro case$ (string-form &rest cases)
  (let ((v$ (gensym))
        (default (or (find 'otherwise cases :key #'car)
                     (find 'otherwise cases :key #'car))))
     (when default
       (setf cases (delete default cases)))
     `(let ((,v$ ,string-form))
         (cond
          ,@(mapcar (lambda (case-forms)
                        `((string-equal ,v$ ,(car case-forms)) ,@(rest case-forms)))
                    cases)
          (t ,@(or (cdr default) `(nil)))))))

(defmacro case-string-equal (string-form &rest cases)
  (let ((v$ (gensym))
        (default (or (find 'otherwise cases :key #'car)
                   (find 'otherwise cases :key #'car))))
    (when default
      (setf cases (delete default cases)))
    `(let ((,v$ ,string-form))
       (cond
        ,@(mapcar (lambda (case-forms)
                    `((string-equal ,v$ ,(string (car case-forms))) ,@(rest case-forms)))
            cases)
        (t ,@(or (cdr default) `(nil)))))))

;--------

(defmethod shortc (other)
  (declare (ignorable other))
  (concatenate 'string "noshortc" (symbol-name (class-name (class-of other)))))

(defmethod longc (other) (shortc other))

(defmethod shortc ((nada null)) nil)
(defmethod shortc ((many list))
   (if (consp (cdr many))
       (mapcar #'shortc many)
     (conc$ (shortc (car many)) " " (shortc (cdr many)))))
(defmethod shortc ((self string)) self)
(defmethod shortc ((self symbol)) (string self))
(defmethod shortc ((self number)) (num$ self))
(defmethod shortc ((self character)) (string self))

;-----------------------

(defun strloc$ (substr str)
   (when (and substr str (not (string= substr "")))
     (search substr str)))

(defun make$ (&optional (size 0) (char #\space))
   (make-string size :initial-element (etypecase char
                                        (character char)
                                        (number (code-char char)))))
(defun basic$ ()
  (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))

(defun space$ (size)
  (make$ size))

(defun char$ (char)
   (make$ 1 char))

(defun conc-list$ (ss)
   (when ss
     (reduce (lambda (s1 s2) (concatenate 'string s1 s2)) ss)))

(defun conc$ (&rest ss)
  (with-output-to-string (stream)
    (dolist (s ss)
      (when s
        (princ (shortc s) stream)))))

(defun left$ (s n)
   (subseq s 0 (max (min n (length s)) 0)))

(export! cc$)
(defun cc$ (code) (string (code-char code)))

(defun mid$ (s offset length)
  (let* ((slen (length s))
         (start (min slen (max offset 0)))
         (end (max start (min (+ offset length) slen))))
   (subseq s start end)))

(defun seg$ (s offset end)
  (let* ((slen (length s))
         (start (min slen (max offset 0)))
         (end (max start (min end slen))))
   (subseq s start end)))

(defun right$ (s n)
   (subseq s (min n (length s))))

(defun insert$ (s c &optional (offset (length s)))
     (conc$ (subseq s 0 offset)
       (string c)
       (subseq s offset)))

(defun remove$ (s offset)
     (conc$ (subseq s 0 (1- offset))
       (subseq s offset)))

(defun trim$ (s)
   (assert (or (null s) (stringp s)))
   (string-trim '(#\space) s))

(defun trunc$ (s char)
   (let ((pos (position char s)))
      (if pos
         (subseq s 0 pos)
         s)))

(defun abbrev$ (long$ max)
  (if (<= (length long$) max)
        long$
      (conc$ (left$ long$ (- max 3)) "...")))

(defmethod empty ((nada null)) t)
(defmethod empty ((c cons))
  (and (empty (car c))
       (empty (cdr c))))
(defmethod empty ((s string)) (empty$ s))
(defmethod empty (other) (declare (ignorable other)) nil)

(defun empty$ (s)
   (or (null s)
       (if (stringp s)
          (string-equal "" (trim$ s))
          #+(or) (format t "empty$> sees non-string ~a" (type-of s)))))

(defmacro find$ (it where &rest args)
  `(find ,it ,where ,@args :test #'string-equal))

(defmethod num$ ((n number))
   (format nil "~d" n))

(defmethod num$ (n)
   (format nil "~d" n))

(defun normalize$ (s)
   (down$ s))

(defun down$ (s)
   (etypecase s
     (null "")
     (string (string-downcase s))
     (number (format nil "~a" s))
     (symbol (string-downcase (symbol-name s)))
     (cons (format nil "~{~(~a~)~^ ~}" s))))

(defun lower$ (s)
   (string-downcase s))

(defun up$ (s)
   (string-upcase s))

(defun upper$ (s)
   (string-upcase s))

(defun equal$ (s1 s2)
   (if (empty$ s1)
      (empty$ s2)
      (when s2
         (string-equal s1 s2))))

(defun min$ (&rest ss)
   (cond
    ((null ss) nil)
    ((null (cdr ss)) (car ss))
    (t (let ((rmin$ (apply #'min$ (cdr ss))))
          (if (string< (car ss) rmin$)
             (car ss) rmin$)))))

(defun numeric$ (s &optional trimmed)
   (every (lambda (c) (digit-char-p c)) (if trimmed (trim$ s) s)))

(defun alpha$ (s)
   (every (lambda (c) (alpha-char-p c)) s))

(defmacro assoc$ (item alist &rest kws)
   `(assoc ,item ,alist :test #'equal ,@kws))

(defmacro member$ (item list &rest kws)
   `(member ,item ,list :test #'string= ,@kws))

(defun starts$ (a b)
  (bwhen (s (search b a))
    (zerop s)))

(defparameter *return$* (conc$ (char$ #\return) (char$ #\linefeed)))
(defparameter *lf$* (string #\linefeed))
