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

(defmacro defmodel (class directsupers slotspecs &rest options)
  ;;(print `(defmodel sees directsupers ,directsupers using ,(or directsupers :model-object)))
  `(progn
     (eval-when (:compile-toplevel :execute :load-toplevel)
       (setf (get ',class :cell-types) nil))
     ;
     ; define slot macros before class so they can appear in initforms and default-initargs
     ;
     ,@(mapcar (lambda (slotspec)
                 (destructuring-bind
                     (slotname &rest slotargs
                       &key (cell t) (accessor slotname) reader
                       &allow-other-keys)
                     slotspec

                   (declare (ignorable slotargs))
                   (when cell
                     (let* ((reader-fn (or reader accessor))
                            (deriver-fn (intern$ "^" (symbol-name reader-fn)))
                            )
                       ;
                       ; may as well do this here...
                       ;
                       ;;(trc nil "slot, deriverfn would be" slotname deriverfn)
                       `(eval-when (:compile-toplevel :execute :load-toplevel)
                          (setf (md-slot-cell-type ',class ',slotname) ,cell)
                          (unless (macro-function ',deriver-fn)
                            (defmacro ,deriver-fn ()
                              `(,',reader-fn self)))
                          )
                       ))
                   ))
         slotspecs)
     
     ;
     ; -------  defclass ---------------  (^slot-value ,model ',',slotname)
     ;
     
     (progn
       (defclass ,class ,(or directsupers '(model-object));; now we can def the class
               ,(mapcar (lambda (s)
                          (list* (car s)
                            (let ((ias (cdr s)))
                              ;; We handle accessor below
                              (when (getf ias :cell t)
                                (remf ias :reader)
                                (remf ias :writer)
                                (remf ias :accessor))
                              (remf ias :cell)
                              (remf ias :unchanged-if)
                              ias))) (mapcar #'copy-list slotspecs))
               (:documentation
                ,@(or (cdr (find :documentation options :key #'car))
                    '("chya")))
               (:default-initargs ;; nil ok and needed: acl oddity in re not clearing d-i's sans this
                   ,@(cdr (find :default-initargs options :key #'car)))
               (:metaclass ,(or (cadr (find :metaclass options :key #'car))
                              'standard-class)))

       (defmethod shared-initialize :after ((self ,class) slot-names &rest iargs &key)
         (declare (ignore slot-names iargs))
         ,(when (and directsupers (not (member 'model-object directsupers)))
            `(unless (typep self 'model-object)
               (error "If no superclass of ~a inherits directly
or indirectly from model-object, model-object must be included as a direct super-class in
the defmodel form for ~a" ',class ',class))))
       ;
       ; slot accessors once class is defined...
       ;
       ,@(mapcar (lambda (slotspec)
                   (destructuring-bind
                       (slotname &rest slotargs
                         &key (cell t) unchanged-if (accessor slotname) reader writer type
                         &allow-other-keys)
                       slotspec

                     (declare (ignorable slotargs))
                     (when cell
                       (let* ((reader-fn (or reader accessor))
                              (writer-fn (or writer accessor))
                              )
                         (setf (md-slot-cell-type class slotname) cell)
                         
                         
                         `(progn
                            ,(when reader-fn
                               `(defmethod ,reader-fn ((self ,class))
                                  (md-slot-value self ',slotname)))
                            
                            ,(when writer-fn
                               `(defmethod (setf ,writer-fn) (new-value (self ,class))
                                  (setf (md-slot-value self ',slotname)
                                    ,(if type
                                         `(coerce new-value ',type)
                                       'new-value))))

                            ,(when unchanged-if
                               `(def-c-unchanged-test (,class ,slotname) ,unchanged-if))
                            )
                         ))
                     ))
           slotspecs)
       (find-class ',class))))
