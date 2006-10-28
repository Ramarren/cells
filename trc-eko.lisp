;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    The Newly Cells-aware TRC trace and EKO value echo facilities

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cells)

;----------- trc -------------------------------------------

(defparameter *trcdepth* 0)

(export! trc wtrc eko)

(defun trcdepth-reset ()
  (setf *trcdepth* 0))


(defmacro trc (tgt-form &rest os)
  (if (eql tgt-form 'nil)
      '(progn)
    (if (stringp tgt-form)
        `(without-c-dependency
          (call-trc t ,tgt-form ,@os))
      (let ((tgt (gensym)))
        `(without-c-dependency
          (bif (,tgt ,tgt-form)
            (if (trcp ,tgt)
                (progn
                  (assert (stringp ,(car os)))
                  (call-trc t ,@os)) ;;,(car os) ,tgt ,@(cdr os)))
              (progn
                ;; (break "trcfailed")
                (count-it :trcfailed)))
            (count-it :tgtnileval)))))))

(export! trcx)

(defmacro trcx (tgt-form &rest os)
  (if (eql tgt-form 'nil)
      '(progn)
    `(without-c-dependency
         (call-trc t ,(format nil "TX> ~(~a~)" tgt-form)
           ,@(loop for obj in os
                   nconcing (list (format nil "~a:" obj) obj))))))


(defparameter *last-trc* (get-internal-real-time))

(defun call-trc (stream s &rest os)
  (if #+cormanlisp nil #-cormanlisp (and (boundp '*trcdepth*)
                                      *trcdepth*)
    (format stream "~&~v,,,'.<~d~>> " (mod *trcdepth* 100) *trcdepth*)
    (format stream "~&"))
  (format stream " ~a " (round (- (get-internal-real-time) *last-trc*) 10))
  (setf *last-trc* (get-internal-real-time))
  (format stream "~a" s)
  (let (pkwp)
    (dolist (o os)
      (format stream (if pkwp " ~(~s~)" " ~(~s~)") o) ;; save, used to insert divider, trcx dont like
      (setf pkwp (keywordp o))))
  (force-output stream)
  (values))
  
(defun call-trc-to-string (fmt$ &rest fmt-args)
    (let ((o$ (make-array '(0) :element-type 'base-char
                :fill-pointer 0 :adjustable t)))
      (with-output-to-string (os-stream o$)
        (apply 'call-trc os-stream fmt$ fmt-args))
      o$))

#+findtrcevalnils
(defmethod trcp :around (other)
  (unless (call-next-method other)(break)))

(export! trcp)

(defmethod trcp (other)
  (eq other t))

(defmethod trcp (($ string))
  t)
  
(defun trcdepth-incf ()
  (incf *trcdepth*))
  
(defun trcdepth-decf ()
  (format t "decrementing trc depth ~d" *trcdepth*)
  (decf *trcdepth*))
  
(export! wtrc eko-if)

(defmacro wtrc ((&optional (min 1) (max 50) &rest banner) &body body )
  `(let ((*trcdepth* (if *trcdepth*
                         (1+ *trcdepth*)
                       0)))
     ,(when banner `(when (>= *trcdepth* ,min)
                      (if (< *trcdepth* ,max)
                          (trc ,@banner)
                        (progn
                          (break "excess trace notttt!!! ~d" *trcdepth*) ;; ,@banner)
                          nil))))
     (when (< *trcdepth* ,max)
       ,@body)))

(defmacro wnotrc ((&optional (min 1) (max 50) &rest banner) &body body )
  (declare (ignore min max banner))
  `(progn ,@body))
  
;------ eko --------------------------------------


(defmacro eko ((&rest trcargs) &rest body)
  (let ((result (gensym)))
     `(let ((,result ,@body))
         (trc ,(car trcargs) :=> ,result ,@(cdr trcargs))
         ,result)))

(defmacro ekx (ekx-id &rest body)
  (let ((result (gensym)))
     `(let ((,result (,@body)))
         (trc ,(string-downcase (symbol-name ekx-id)) :=> ,result)
         ,result)))

(export! ekx)

(defmacro eko-if ((&rest trcargs) &rest body)
  (let ((result (gensym)))
     `(let ((,result ,@body))
         (when ,result
           (trc ,(car trcargs) :res ,result ,@(cdr trcargs)))
         ,result)))

(defmacro ek (label &rest body)
  (let ((result (gensym)))
     `(let ((,result (,@body)))
         (when ,label
           (trc ,label ,result))
         ,result)))