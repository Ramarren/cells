;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

#+(or allegro lispworks cmu mcl clisp cormanlisp sbcl scl)
(progn
(declaim (optimize (debug 3) (speed 3) (safety 1) (compilation-speed 0)))

(asdf:defsystem :cells
    :name "cells"
  :author "Kenny Tilton <kentilton@gmail.com>"
  :maintainer "Kenny Tilton <kentilton@gmail.com>"
  :licence "MIT Style"
  :description "Cells"
  :long-description "Cells: a dataflow extension to CLOS."
  :serial t
  :components ((:module "utils-kt"
                 :serial t
                 :components ((:file "defpackage")
                              (:file "debug")
                              (:file "flow-control")
                              (:file "detritus")
                              (:file "strings")
                              (:file "datetime")))
               (:file "defpackage")
               (:file "cells")
               (:file "integrity")
               (:file "constructors")
               (:file "cell-types")
               (:file "synapse")
               (:file "synapse-types")
               (:file "initialize")
               (:file "md-slot-value")
               (:file "slot-utilities")
               (:file "optimization")
               (:file "link")
               (:file "propagate")
               (:file "model-object")
               (:file "defmodel")
               (:file "md-utilities")
               (:file "family")
               (:file "fm-utilities")
               (:file "family-values")))

(defmethod perform ((o load-op) (c (eql (find-system :cells))))
  (pushnew :cells *features*))

(defmethod perform ((o test-op) (c (eql (find-system :cells))))
  (oos 'load-op :cells-test))

(defmethod perform ((o test-op) (c (eql :cells)))
  (oos 'load-op :cells-test))

)