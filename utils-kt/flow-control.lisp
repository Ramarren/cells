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

(export! list-flatten! tree-flatten list-insertf subseq-contiguous-p)

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
  
(defmacro bif ((bindvar boundform) yup &optional nope)
  `(let ((,bindvar ,boundform))
      (if ,bindvar
         ,yup
         ,nope)))

(defmacro maptimes ((nvar count) &body body)
  `(loop for ,nvar below ,count
       collecting (progn ,@body)))

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

