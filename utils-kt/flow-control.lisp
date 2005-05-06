;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils-kt; -*-
;;; 
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

(in-package :utils-kt)

(defun last1 (thing)
     (car (last thing)))

(defun max-if (&rest values)
  (loop for x in values when x maximize x))

(defun min-max-of (v1 v2)
  (values (min-if v1 v2) (max-if v1 v2)))

(defun min-if (v1 v2)
     (if v1 (if v2 (min v1 v2) v1) v2))

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

(defun packed-flat! (&rest u-nameit)
   (delete-if #'null (list-flatten! u-nameit)))

(defmacro with-dynamic-fn ((fn-name (&rest fn-args) &body fn-body) &body body)
  `(let ((,fn-name (lambda ,fn-args ,@fn-body)))
     (declare (dynamic-extent ,fn-name))
     ,@body))

(defun intern$ (&rest strings)
  (intern  (apply #'concatenate 'string (mapcar #'string-upcase strings))))

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

