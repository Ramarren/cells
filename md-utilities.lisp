;; -*- mode: Lisp; Syntax: Common-Lisp; Package: cells; -*-
#|

    Cells -- Automatic Dataflow Managememnt

Copyright (C) 1995, 2006 by Kenneth Tilton

This library is free software; you can redistribute it and/or
modify it under the terms of the Lisp Lesser GNU Public License
 (http://opensource.franz.com/preamble.html), known as the LLGPL.

This library is distributed  WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

See the Lisp Lesser GNU Public License for more details.

|#

(in-package :cells)

(defun md-awake (self) (eql :awake (md-state self)))

(defun fm-grandparent (md)
  (fm-parent (fm-parent md)))


(defmethod md-release (other)
  (declare (ignorable other)))

(export! mdead)
;___________________ birth / death__________________________________
  
(defgeneric mdead (self)
  (:method ((self model-object))
    (unless *not-to-be*
      (eq :eternal-rest (md-state self))))

  (:method (self)
    (declare (ignore self))
    nil))

(defgeneric not-to-be (self)
  (:method ((self model-object))
    (md-quiesce self))

  (:method :around ((self model-object))
    (declare (ignorable self))
    (let ((*not-to-be* t))
      (trc nil #+not (typep self '(or mathx::problem mathx::prb-solvers mathx::prb-solver))
        "not.to-be nailing" self)
      (unless (eq (md-state self) :eternal-rest)
        (call-next-method)
        
        (setf (fm-parent self) nil
          (md-state self) :eternal-rest)

        (md-map-cells self nil
          (lambda (c)
            (c-assert (eq :quiesced (c-state c))))) ;; fails if user obstructs not.to-be with primary method (use :before etc)

        (trc nil "not.to-be cleared 2 fm-parent, eternal-rest" self)))))

(defun md-quiesce (self)
  (trc nil "md-quiesce nailing cells" self (type-of self))
  (md-map-cells self nil (lambda (c)
                           (trc nil "quiescing" c)
                           (c-assert (not (find c *call-stack*)))
                           (c-quiesce c))))

(defun c-quiesce (c)
  (typecase c
    (cell 
     (trc nil "c-quiesce unlinking" c)
     (c-unlink-from-used c)
     (dolist (caller (c-callers c))
       (setf (c-value-state caller) :uncurrent)
       (trc nil "c-quiesce unlinking caller and making uncurrent" :q c :caller caller)
       (c-unlink-caller c caller))
     (setf (c-state c) :quiesced) ;; 20061024 for debugging for now, might break some code tho
     )))

(defparameter *to-be-dbg* nil)

(defmacro make-kid (class &rest initargs)
  `(make-instance ,class
     ,@initargs
     :fm-parent (progn (assert self) self)))


;;;
;;; cells store stuff
;;;    (w) Peter Hildebrandt

(export! cells-store bwhen-c-stored c?-with-stored with-store-item store-add store-lookup store-remove)

(defmacro c?-with-stored ((var key store &optional default) &body body)
  `(c? (bwhen-c-stored (,var ,key ,store ,default)
	 ,@body)))

(defmacro with-uniqs ((&rest symbols) &body body)
  `(let ,(mapcar #'(lambda (sym) `(,sym (gensym ,(string sym)))) symbols)
     ,@body))

(defmacro bwhen-c-stored ((var key store &optional if-not) &body body)
  (with-uniqs (gkey gstore glink gifnot)
    `(let ((,gkey ,key)
	   (,gstore ,store)
	   (,gifnot ,if-not))
	(let ((,glink (query-c-link ,gkey ,gstore)))
	  (declare (ignorable ,glink))
	  (trc nil "executing bwhen-c-stored" self :update-tick ,glink :lookup (store-lookup ,gkey ,gstore))
	  (bif (,var (store-lookup ,gkey ,gstore))
	       (progn
		 ,@body)
	       ,gifnot)))))

(defmodel cells-store (family)
  ((data :accessor data :initarg :data :cell nil))
  (:default-initargs
      :data (make-hash-table)))

;;; infrastructure for manipulating the store and kicking rules

(defmethod entry (key (store cells-store))
  (gethash key (data store)))

(defmethod (setf entry) (new-data key (store cells-store))
  (setf (gethash key (data store)) new-data))

(defmethod c-link (key (store cells-store))
  (car (entry key store)))

(defmethod (setf c-link) (new-c-link key (store cells-store))
  (if (consp (entry key store))
      (setf (car (entry key store)) new-c-link)
      (setf (entry key store) (cons new-c-link nil)))
  new-c-link)

(defmethod item (key (store cells-store))
  (cdr (entry key store)))

(defmethod (setf item) (new-item key (store cells-store))
  (if (consp (entry key store))
      (setf (cdr (entry key store)) new-item)
      (setf (entry key store) (cons nil new-item)))
  new-item)

;;; c-links

(defmodel c-link ()
  ((value :accessor value :initform (c-in 0) :initarg :value)))

(defmethod query-c-link (key (store cells-store))
  (trc "c-link> query link" key store (c-link key store))
  (value (or (c-link key store)
	     (setf (c-link key store) (make-instance 'c-link)))))

(defmethod kick-c-link (key (store cells-store))
  (bwhen (link (c-link key store))
    (trc "c-link> kick link" key store link)
    (with-integrity (:change :kick-c-link)
     (incf (value link)))))

(defmacro with-store-item ((item key store) &body body)
  `(prog1
       (symbol-macrolet ((,item '(item key store)))
	(progn
	  ,@body))
     (kick-c-link ,key ,store)))


(defmacro with-store-entry ((key store &key quiet) &body body)
  `(prog1
       (progn
	 ,@body)
     (unless ,quiet
       (kick-c-link ,key ,store))))

;;; item management

(defmethod store-add (key (store cells-store) object &key quiet)
  (with-store-entry (key store :quiet quiet)
    (when (item key store)
      (trc "overwriting item" key (item key store)))
    (setf (item key store) object)))

(defmethod store-lookup (key (store cells-store) &optional default)
  (when (mdead (item key store))
    (with-store-entry (key store)
      (trc "looked up dead item -- resetting to nil" key store)
      (setf (item key store) nil)))
  (or (item key store) default))

(defmethod store-remove (key (store cells-store) &key quiet)
  (with-store-entry (key store :quiet quiet)
    (setf (item key store) nil)))


;;;  unit test

(export! test-cells-store)

(defmodel test-store-item (family)
  ())

(defvar *observers*)

(defobserver .value ((self test-store-item))
  (trc "    changed value" :self self :to (value self))
  (when (boundp '*observers*)
    (push self *observers*)))

(defmacro with-assert-observers ((desc &rest asserted-observers) &body body)  
  `(let ((*observers* nil))
     (trc ,desc " -- checking observers")
     ,@body
     (let ((superflous-observers (loop for run in *observers* if (not (member run (list ,@asserted-observers))) collect run))
	   (failed-observers (loop for asserted in (list ,@asserted-observers) if (not (member asserted *observers*)) collect asserted)))
       (trc "called observers on" *observers* :superflous superflous-observers :failed failed-observers)
       (assert (not superflous-observers))
       (assert (not failed-observers)))))

(defmacro assert-values ((desc) &body objects-and-values)
  `(progn
     (trc ,desc)
     ,@(loop for (obj val) in objects-and-values
	    collect `(assert (eql (value ,obj) ,val)))))

(defun test-cells-store ()
  (trc "testing cells-store -- making objects")
  (let* ((store (make-instance 'cells-store))
	 (foo (make-instance 'test-store-item :value (c?-with-stored (v :foo store 'nothing)
						       (bwhen (val (value v)) val))))
	 (foo+1 (make-instance 'test-store-item :value (c?-with-stored (v :foo store 'nothing)
							 (bwhen (val (value v)) (1+ val)))))
	 (bar (make-instance 'test-store-item :value (c?-with-stored (v :bar store 'nothing)
						       (bwhen (val (value v)) val))))
	 (bar-1 (make-instance 'test-store-item :value (c?-with-stored (v :bar store 'nothing)
							 (bwhen (val (value v)) (1- val)))))
	 (bypass-lookup? (make-instance 'family :value (c-in t)))
	 (baz (make-instance 'test-store-item :value (c? (if (value bypass-lookup?)
							     'no-lookup
							     (bwhen-c-stored (v :bar store 'nothing)
							       (value v)))))))

    (assert-values ("assert fresh initialization")
      (foo 'nothing)
      (foo+1 'nothing)
      (bar 'nothing)
      (bar-1 'nothing))

    (with-assert-observers ("adding foo" foo foo+1)
      (store-add :foo store (make-instance 'family :value (c-in nil))))

    (assert-values ("added foo = nil")
      (foo nil)
      (foo+1 nil)
      (bar 'nothing)
      (bar-1 'nothing))
    
    (with-assert-observers ("changing foo" foo foo+1)
      (setf (value (store-lookup :foo store)) 1))

    (assert-values ("changed foo = 1")
      (foo 1)
      (foo+1 2)
      (bar 'nothing)
      (bar-1 'nothing))
   
    (with-assert-observers ("adding bar = 42" bar bar-1)
      (store-add :bar store (make-instance 'family :value (c-in 42))))

    (assert-values ("changed foo = 1")
      (foo 1)
      (foo+1 2)
      (bar 42)
      (bar-1 41))
    
    (with-assert-observers ("changing bar to 2" bar bar-1)
      (setf (value (store-lookup :bar store)) 2))

    (assert-values ("changed foo = 1")
      (foo 1)
      (foo+1 2)
      (bar 2)
      (bar-1 1))

    (assert-values ("baz w/o lookup")
      (baz 'no-lookup))

    (with-assert-observers ("activating lookup" baz)
      (setf (value bypass-lookup?) nil))

    (assert-values ("baz w/lookup")
      (baz 2))

    (with-assert-observers ("deleting foo" foo foo+1)
      (store-remove :foo store))

    (assert-values ("deleted foo")
      (foo 'nothing)
      (foo+1 'nothing)
      (bar 2)
      (bar-1 1))

    (with-assert-observers ("deleting bar" bar bar-1 baz)
      (store-remove :bar store))

    (assert-values ("deleted bar")
      (foo 'nothing)
      (foo+1 'nothing)
      (bar 'nothing)
      (bar-1 'nothing)
      (baz 'nothing))

    (with-assert-observers ("de-activating lookup" baz)
      (setf (value bypass-lookup?) t))

    (assert-values ("baz w/o lookup")
      (baz 'no-lookup))))