;; -*- mode: Lisp; Syntax: Common-Lisp; Package: utils-kt; -*-
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

(defmacro wdbg (&body body)
  `(let ((*dbg* t))
     ,@body))

;;;(defmethod class-slot-named ((classname symbol) slotname)
;;;  (class-slot-named (find-class classname) slotname))
;;;
;;;(defmethod class-slot-named (class slotname)
;;;  (find slotname (class-slots class) :key #'slot-definition-name))

#+(and mcl (not openmcl-partial-mop))
(defun class-slots (c)
  (nconc (copy-list (class-class-slots c))
         (copy-list (class-instance-slots c))))


#-(or lispworks mcl)
(progn
  (defun true (it) (declare (ignore it)) t)
  (defun false (it) (declare (ignore it))))

(defun xor (c1 c2)
  (if c1 (not c2) c2))

;;; --- FIFO Queue -----------------------------

(defun make-fifo-queue (&rest init-data)
  (let ((q (cons nil nil)))
    (prog1 q
      (loop for id in init-data
            do (fifo-add q id)))))

(deftype fifo-queue () 'cons)

(defun fifo-data (q) (car q))
(defun fifo-clear (q) (rplaca q nil))
(defun fifo-empty (q) (not (fifo-data q)))
(defun fifo-length (q) (length (fifo-data q)))
(defun fifo-peek (q) (car (fifo-data q)))

(defun fifo-browse (q fn)
  (map nil fn (fifo-data q)))

(defun fifo-add (q new)
  (if (car q)
      (let ((last (cdr q))
            (newlast (list new)))
        (rplacd last newlast)
        (rplacd q newlast))
    (let ((newlist (list new)))
      (rplaca q newlist)
      (rplacd q newlist))))

(defun fifo-delete (q dead)
  (let ((c (member dead (fifo-data q))))
    (assert c)
    (rplaca q (delete dead (fifo-data q)))
    (when (eq c (cdr q))
      (rplacd q (last (fifo-data q))))))

(defun fifo-pop (q)
  (unless (fifo-empty q)
    (prog1
        (fifo-peek q)
      (rplaca q (cdar q)))))

(defun fifo-map (q fn)
  (loop until (fifo-empty q)
      do (funcall fn (fifo-pop q))))

(defmacro with-fifo-map ((pop-var q) &body body)
  (let ((qc (gensym)))
    `(loop with ,qc = ,q
         while (not (fifo-empty ,qc))
         do (let ((,pop-var (fifo-pop ,qc)))
              ,@body))))

#+(or)
(let ((*print-circle* t))
  (let ((q (make-fifo-queue)))
    (loop for n below 3
      do (fifo-add q n))
    (fifo-delete q 1)
    (loop until (fifo-empty q)
          do (print (fifo-pop q)))))

(defmacro define-constant (name value &optional docstring)
  "Define a constant properly.  If NAME is unbound, DEFCONSTANT
it to VALUE.  If it is already bound, and it is EQUAL to VALUE,
reuse the SYMBOL-VALUE of NAME.  Otherwise, DEFCONSTANT it again,
resulting in implementation-specific behavior."
  `(defconstant ,name
     (if (not (boundp ',name))
	 ,value
	 (let ((value ,value))
	   (if (equal value (symbol-value ',name))
	       (symbol-value ',name)
	       value)))
     ,@(when docstring (list docstring))))

#+allegro
(defun line-count (path &optional show-files (depth 0))
  (cond
   ((excl:file-directory-p path)
    (when show-files
      (format t "~&~v,8t~a counts:" depth (pathname-directory path)))
    (let ((directory-lines          
           (loop for file in (directory path :directories-are-files nil)
               for lines = (line-count file show-files (1+ depth))
               when (and show-files (plusp lines))
               do (bwhen (fname (pathname-name file))
                    (format t "~&~v,8t~a ~,40t~d" (1+ depth) fname lines))
               summing lines)))
      (format t "~&~v,8t~a ~,50t~d" depth (pathname-directory path) directory-lines)
      directory-lines))

   ((find (pathname-type path) '("cl" "lisp" "c" "h" "java")
      :test 'string-equal)
    (source-line-count path))
   (t 0)))

(defun source-line-count (path)
   (with-open-file (s path)
     (loop with lines = 0
         for c = (read-char s nil nil)
         while c
         when (find c '(#\newline #\return))
         do (incf lines)
         finally (return lines))))

#+(or)
(line-count (make-pathname
             :device "c"
             :directory `(:absolute "0dev" "Algebra")) t)

