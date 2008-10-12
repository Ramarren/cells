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
  (export '(eval-now! export! assocd rassoca class-proto brk)))

(defmacro wdbg (&body body)
  `(let ((*dbg* t))
     ,@body))

(defun assocd (x y) (cdr (assoc x y)))
(defun rassoca (x y) (car (assoc x y)))

(defun class-proto (c)
  (let ((cc (find-class c)))
    (when cc
      (finalize-inheritance cc))
    (mop::class-prototype cc)))


(defun brk (&rest args)
  #+its-alive! (apply 'error args)
  #-its-alive! (progn
                 ;;(setf *ctk-dbg* t)
                 (apply 'break args)))

(defun find-after (x l)
  (bIf (xm (member x l))
    (cadr xm)
    (brk "find-after ~a not member of ~a" x l)))

(defun find-before (x l)
  (loop with prior = nil
        for i in l
        if (eql i x)
        return prior
        else do (setf prior i)
        finally (brk "find-before ~a not member of ~a" x l)))

(defun list-insert-after (list after new )
  (let* ((new-list (copy-list list))
         (m (member after new-list)))
    (rplacd m (cons new (cdr m)))
    new-list))

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

(export! collect collect-if find-after find-before list-insert-after)

(defun collect (x list &key (key 'identity) (test 'eql))
  (loop for i in list
        when (funcall test x (funcall key i))
        collect i))

(defun collect-if (test list)
  (remove-if-not test list))

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

#+test
(line-count "/openair" t 10 t)

#+allegro
(defun line-count (path &optional show-files (max-depth most-positive-fixnum) no-semis (depth 0))
  (cond
   ((excl:file-directory-p path)
    (if (>= depth max-depth)
        (progn
          (format t "~&~v,8t~a dir too deep:" depth (pathname-directory path))
          0)
      (progn
        (when show-files
          (format t "~&~v,8t~a counts:" depth (pathname-directory path)))
        (let ((directory-lines          
               (loop for file in (directory path :directories-are-files nil)
                   for lines = (line-count file show-files max-depth no-semis (1+ depth))
                   when (and show-files (plusp lines))
                   do (bwhen (fname (pathname-name file))
                        (format t "~&~v,8t~a ~,40t~d" (1+ depth) fname lines))
                   summing lines)))
          (unless (zerop directory-lines)
            (format t "~&~v,8t~a ~,50t~d" depth (pathname-directory path) directory-lines))
          directory-lines))))

   ((find (pathname-type path) '("cl" "lisp" "c" "h" "java")
      :test 'string-equal)
    (source-line-count path no-semis))
   (t 0)))

(defun source-line-count (path no-semis)
  (with-open-file (s path)
    (loop with block-rem = 0
        for line = (read-line s nil nil)
        for trim = (when line (string-trim '(#\space #\tab) line))
        while line
        when (> (length trim) 1)
        do (cond
            ((string= "#|" (subseq trim 0 2))(incf block-rem))
            ((string= "|#" (subseq trim 0 2))(decf block-rem)))
        unless (or (string= trim "")
                 (and no-semis (or (plusp block-rem)
                                 (char= #\; (schar trim 0)))))
        count 1)))

#+(or)
(line-count (make-pathname
             :device "c"
             :directory `(:absolute "0algcount" ))
  nil 5 t)

#+(or)
(loop for d1 in '("cl-s3" "kpax" "puri-1.5.1" "s-base64" "s-http-client" "s-http-server" "s-sysdeps" "s-utils" "s-xml")
      summing (line-count (make-pathname
                      :device "c"
                      :directory `(:absolute "0Algebra" "1-devtools" ,d1))))


(export! tree-includes tree-traverse tree-intersect)

(defun tree-includes (sought tree &key (test 'eql))
  (typecase tree
    (null)
    (atom (funcall test sought tree))
    (cons (or (tree-includes sought (car tree) :test test)
            (tree-includes sought (cdr tree) :test test)))))

(defun tree-traverse (tree fn)
  (typecase tree
    (null)
    (atom (funcall fn tree))
    (cons (tree-traverse (car tree) fn)
      (tree-traverse (cdr tree) fn)))
  (values))

(defun tree-intersect (t1 t2 &key (test 'eql))
  (tree-traverse t1
    (lambda (t1-node)
      (when (tree-includes t1-node t2 :test test)
          (return-from tree-intersect t1-node)))))

