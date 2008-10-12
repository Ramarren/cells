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

(defun last1 (thing)
     (car (last thing)))

(defun max-if (&rest values)
  (loop for x in values when x maximize x))

(defun min-max-of (v1 v2)
  (values (min-if v1 v2) (max-if v1 v2)))

(defun min-if (v1 v2)
     (if v1 (if v2 (min v1 v2) v1) v2))

(export! list-flatten! tree-flatten list-insertf subseq-contiguous-p pair-off)

(defun list-flatten! (&rest list)
  (if (consp list)
    (let (head work visited)
      (labels ((link (cell)
                 ;;(format t "~&Link > cons: ~s . ~s" (car cell) (cdr cell))
                 (when (and (consp cell)
                            (member cell visited))
                   (break "list-flatten! detects infinite list: cell ~a, visited ~a" cell visited))
                 (push cell visited)
                 
                 (when cell
                   (if (consp (car cell))
                      (link (car cell))
                      (progn
                       (setf head (or head cell))
                       (when work
                          (rplacd work cell))
                       (setf work cell)))
                   (link (rest cell)))))
        (link list))
      head)
    list))

(defun tree-flatten (tree)
  (list-flatten! (copy-tree tree)))

(export! push-end)
(defmacro push-end (item place )
  `(setf ,place (nconc ,place (list ,item))))

(defun pair-off (list &optional (test 'eql))
  (loop with pairs and copy = (copy-list list)
      while (cdr copy)
      do (let ((pair (find (car copy) (cdr copy) :test test)))
           (if pair
               (progn
                 (push-end (cons (car copy) pair) pairs)
                 (setf copy (delete pair (cdr copy) :count 1)))
             (setf copy (cdr copy))))
      finally (return pairs)))

(defun packed-flat! (&rest u-nameit)
  (delete nil (list-flatten! u-nameit)))

(defmacro with-dynamic-fn ((fn-name (&rest fn-args) &body fn-body) &body body)
  `(let ((,fn-name (lambda ,fn-args ,@fn-body)))
     (declare (dynamic-extent ,fn-name))
     ,@body))

(defmacro list-insertf (place item &key after)
  (let ((list (gensym))
        (afterv (gensym))
        (afters (gensym)))
    `(let* ((,list ,place)
            (,afterv ,after)
            (,afters (when ,afterv (member ,after ,list))))
       (assert (or (null ,afterv) ,afters) () "list-insertf after ~a not in list ~a" ,afterv ,list)
       (setf ,place
         (if ,afterv
             (append (ldiff ,list ,afters)
               (list ,afterv)
               (list ,item)
               (cdr ,afters))
           (append ,list (list ,item)))))))

(defun intern$ (&rest strings)
  (intern  (apply #'concatenate 'string strings)))

#-allegro
(defmacro until (test &body body)
  `(loop (when ,test (return)) ,@body))

#-allegro
(defmacro while (test &body body)
  `(loop (unless ,test (return)) ,@body))

(defmacro bwhen ((bindvar boundform) &body body)
  `(let ((,bindvar ,boundform))
      (when ,bindvar
        ,@body)))

(defmacro b-when (bindvar boundform &body body)
  `(let ((,bindvar ,boundform))
     (when ,bindvar
       ,@body)))
  
(defmacro bif ((bindvar boundform) yup &optional nope)
  `(let ((,bindvar ,boundform))
      (if ,bindvar
         ,yup
         ,nope)))

(defmacro b-if (bindvar boundform yup &optional nope)
  `(let ((,bindvar ,boundform))
     (if ,bindvar
         ,yup
       ,nope)))

(defmacro b1 ((bindvar boundform) &body body)
  `(let ((,bindvar ,boundform))
     ,@body))

(defmacro maptimes ((nvar count) &body body)
  `(loop for ,nvar below ,count
       collecting (progn ,@body)))

(export! b1 maphash* hashtable-assoc -1?1 -1?1 prime? b-if b-when)

(defun maphash* (f h)
  (loop for k being the hash-keys of h
        using (hash-value v)
        collecting (funcall f k v)))

(defun hashtable-assoc (h)
  (maphash* (lambda (k v) (cons k v)) h))

(define-symbol-macro -1?1 (expt -1 (random 2)))

(defun -1?1 (x) (* -1?1 x))

(defun prime? (n)
  (when (> n 1)
    (cond
     ((= 2 n) t)
     ((evenp n) (values nil 2))
     (t (loop for d upfrom 3 by 2 to (sqrt n)
            when (zerop (mod n d)) do (return-from prime? (values nil d))
            finally (return t))))))



; --- cloucell support for struct access of slots ------------------------

(eval-when (:compile-toplevel :execute :load-toplevel)
  (export '(cc-defstruct instance-slots)))

(defmacro cc-defstruct (header &rest slots)
  (let (name conc-name (cache (gensym)))
    (if (consp header)
        (destructuring-bind (hname &rest options)
            header
          (setf name hname)
          (setf conc-name (bif (conc-option (find :conc-name options :key #'car))
                           (unless (eql (second conc-option) 'nil)
                             (second conc-option))
                           (intern (concatenate 'string
                               (symbol-name hname)
                               "-")))))
      (progn
        (setf name header)
        (setf conc-name (intern (concatenate 'string
                               (symbol-name header) "-")))))

    (let ((cc-info (mapcar (lambda (s)
                              (let ((sn (if (consp s)
                                            (car s) s)))
                                (cons sn
                                  (intern (concatenate 'string
                                            (when conc-name (symbol-name conc-name))
                                            (symbol-name sn))))))
                      slots)))
    `(progn
       (defstruct ,header ,@slots)
       (let (,cache)
         (defmethod instance-slots ((self ,name))
           (or ,cache (setf ,cache (append (call-next-method) ',cc-info)))))
       ))))

(defmethod instance-slots (self)
  (class-slots (class-of self))) ;; acl has this for structs

;;; ---- without-repeating ----------------------------------------------

;; Returns a function that generates an elements from ALL each time it
;; is called. When a certain element is generated it will take at
;; least DECENT-INTERVAL calls before it is generated again.  
;;
;; note: order of ALL is important for first few calls, could be fixed

(defun without-repeating-generator (decent-interval all)
  (let ((len (length all))
        (head (let ((v (shuffle all)))
                (nconc v v))))
    (lambda ()
      ;(print (list "without-repeating-generator sees len all =" len :decent-interval decent-interval))
      (if (< len 2)
          (car all)
        (prog2
          (rotatef (car head)
            (car (nthcdr (random (- len decent-interval))
                   head)))
            (car head)
          (setf head (cdr head)))))))

(defun shuffle (list &key (test 'identity))
  (if (cdr list)
      (loop thereis
            (funcall test
              (mapcar 'cdr
                (sort (loop for e in list collecting (cons (random most-positive-fixnum) e))
                  '< :key 'car))))
    (copy-list list)))

(export! without-repeating shuffle)

(defparameter *without-repeating-generators* nil)

(defun reset-without-repeating ()
  (if *without-repeating-generators*
      (clrhash *without-repeating-generators*)
    (setf *without-repeating-generators* (make-hash-table :test 'equalp))))

(defun without-repeating (key all &optional (decent-interval (floor (length all) 2)))
  (funcall (or (gethash key *without-repeating-generators*)
             (progn
               ;(print (list "without-repeating makes new gen" key :all-len (length all) :int decent-interval))
               (setf (gethash key *without-repeating-generators*)
                 (without-repeating-generator decent-interval all))))))

